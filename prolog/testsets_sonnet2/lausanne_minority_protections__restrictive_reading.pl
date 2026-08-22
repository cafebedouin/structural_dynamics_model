% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Restrictive Reading of Lausanne Minority Protections (Individual-Worship-Only)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the restrictive reading of the Lausanne
 *   minority-protections kernel: Section III of the 1923 Treaty of Lausanne
 *   is read domestically as guaranteeing only individual freedom of worship
 *   for non-Muslim minorities, with institutional self-administration,
 *   property ownership by religious foundations, and theological/clergy
 *   education treated as ordinary domestic matters governed by general
 *   Turkish civil, foundations, and education law rather than by
 *   treaty-protected minority institutional rights. Under this reading the
 *   1936 foundations declaration regime, subsequent confiscation statutes,
 *   and the 1971 closure of the Halki Seminary under general higher-education
 *   law are lawful exercises of domestic sovereignty rather than treaty
 *   violations. The sibling readings (expansive: functional continuity of
 *   pre-1923 institutional autonomy; guarantor: internationally supervised
 *   and enforceable obligations) are NOT part of this constraint — they are
 *   separate constraint stories with their own epsilon and stakeholder
 *   structure, linked here only via network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - turkish_state_apparatus: agenda_setter (institutional/arbitrage) — administers and enforces the restrictive domestic reading
 *   - treasury_directorate_general_foundations: beneficiary (institutional/arbitrage) — receives confiscated/escheated minority foundation property
 *   - greek_orthodox_patriarchate: primary target (moderate/trapped) — loses seminary, property, institutional continuity
 *   - armenian_apostolic_community, jewish_community_istanbul, syriac_orthodox_community: co-targets across scale of organizational power
 *   - guarantor_states_and_ecumenical_patriarchate_diplomacy: excluded voice — raises the issue internationally but is ruled non-justiciable domestically
 *   - international_law_scholars_and_treaty_bodies: analytical observer — documents the gap between treaty text and domestic application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.81).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.78).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Restrictive Reading of Lausanne Minority Protections (Individual-Worship-Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'd30030d5-3516-4a4a-8a01-2c3b1a764356').
narrative_ontology:cs_kernel_codification('d30030d5-3516-4a4a-8a01-2c3b1a764356', fixed_text).
narrative_ontology:cs_authority_grounding('d30030d5-3516-4a4a-8a01-2c3b1a764356', extraction).
narrative_ontology:cs_interpretation_layer_present('d30030d5-3516-4a4a-8a01-2c3b1a764356').
narrative_ontology:cs_reading_relation('d30030d5-3516-4a4a-8a01-2c3b1a764356', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('d30030d5-3516-4a4a-8a01-2c3b1a764356', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('d30030d5-3516-4a4a-8a01-2c3b1a764356', foundational, minority_protection_limited_to_individual_worship).
narrative_ontology:cs_axiom_status(minority_protection_limited_to_individual_worship, holdable).
narrative_ontology:cs_axiom_grounding('d30030d5-3516-4a4a-8a01-2c3b1a764356', minority_protection_limited_to_individual_worship, conventional).
narrative_ontology:cs_axiom('d30030d5-3516-4a4a-8a01-2c3b1a764356', foundational, domestic_sovereignty_governs_institutional_and_property_matters).
narrative_ontology:cs_axiom_status(domestic_sovereignty_governs_institutional_and_property_matters, holdable).
narrative_ontology:cs_axiom_grounding('d30030d5-3516-4a4a-8a01-2c3b1a764356', domestic_sovereignty_governs_institutional_and_property_matters, conventional).
narrative_ontology:cs_reference_frame('d30030d5-3516-4a4a-8a01-2c3b1a764356', id_1923_treaty_settlement_as_individual_rights_instrument).
narrative_ontology:cs_drift_state('d30030d5-3516-4a4a-8a01-2c3b1a764356', post_1971_seminary_closure_and_echr_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d30030d5-3516-4a4a-8a01-2c3b1a764356', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, treasury_directorate_general_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, greek_orthodox_patriarchate).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_apostolic_community).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, jewish_community_istanbul).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, syriac_orthodox_community).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_theological_seminaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the domestic legal regime that reads Lausanne Article 40's protections as covering only individual worship, not institutional continuity. Enforces this reading through the courts, the Directorate General of Foundations, and land registry law, treating minority religious institutions as ordinary domestic legal persons (or non-persons) subject to general Turkish civil and foundations law rather than as protected successor institutions to the Ottoman-era millet system.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% Administers seized and escheated minority foundation ('vakıf') properties under 1936 declaration requirements and subsequent confiscation laws justified by the restrictive reading. Property reverting to Treasury or municipal ownership when foundations are deemed to lack continuing legal personality under domestic law flows directly to this agency's asset base.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, treasury_directorate_general_foundations, beneficiary,
    institutional, generational, arbitrage, national).

% Operates the Halki Seminary, closed since 1971 under laws applying general educational statutes to private higher education, closing the only domestic path to clergy formation. Cannot hold real property as a religious institution under this reading (only individually or through foundations vulnerable to confiscation), and has had hundreds of properties reverted to the Treasury. Its ecumenical status is unrecognized domestically; it cannot leave Istanbul without abandoning its seat, and cannot train successor clergy without foreign ordination workarounds of contested legal standing.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, greek_orthodox_patriarchate, payer,
    moderate, civilizational, trapped, national).

% Holds property and runs schools through a patchwork of foundation structures constantly exposed to declaration-based confiscation, since the restrictive reading treats these institutions as ordinary foundations rather than protected minority institutions with guaranteed continuity. Community members can emigrate individually but the institutional infrastructure (churches, schools, cemeteries, hospitals) cannot be relocated and is administratively vulnerable in place.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_apostolic_community, payer,
    moderate, generational, constrained, national).

% Manages synagogues, schools, and welfare foundations under the same restrictive foundations-law regime; faces recurring property title disputes and periodic obligations to prove continuous legal personality that the international-guarantee reading would have made unnecessary. Community has shrunk substantially through emigration, which itself weakens the institutional base's capacity to contest confiscations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, jewish_community_istanbul, payer,
    moderate, generational, constrained, national).

% A smaller, geographically concentrated (largely southeastern Anatolia and Istanbul) community without patriarchal-level diplomatic standing comparable to the Greek or Armenian churches. Monastery and village church properties face land-registry disputes decided under general property law with no minority-protection carve-out, and lacks the international visibility that has occasionally slowed confiscation actions against larger communities.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, syriac_orthodox_community, payer,
    powerless, generational, trapped, regional).

% Institutions (Halki and equivalents) rendered non-operational because theological education is classified as an ordinary domestic educational matter subject to general higher-education licensing law rather than a protected minority institutional right. Their closure forecloses domestic clergy succession entirely, forcing reliance on foreign-trained clergy of contested recognition.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_theological_seminaries, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(lausanne_minority_protections__restrictive_reading, minority_theological_seminaries).

% Greece, other Lausanne signatory states, the Council of Europe, and the European Court of Human Rights raise the institutional-continuity and property questions in diplomatic and litigation fora, but under the restrictive reading these are treated by Turkish courts as non-justiciable domestic matters, keeping this voice structurally outside the domestic adjudicative process even when it is loudly present internationally.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states_and_ecumenical_patriarchate_diplomacy, excluded,
    powerful, generational, constrained, continental).

% Study the divergence between the treaty text's protection of 'minorities' as such (Section III, Articles 37-45) and the domestic reading that narrows this to individual worship, producing scholarship, ECtHR submissions, and comparative treaty-interpretation analysis without power to compel a reading.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_law_scholars_and_treaty_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, treasury_directorate_general_foundations).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The restrictive reading does perform one real coordination function: it establishes a single, general domestic legal framework (foundations law, education law, property law) applicable to all civil-society organizations, avoiding a parallel bespoke legal regime for each minority institution and giving the state one administrative apparatus to manage religious and educational entities generally.
% TRANSFER_FUNCTION: Moves institutional legal personality, real property, and educational infrastructure from minority religious communities to the Turkish state (directly, via Treasury/Directorate General of Foundations confiscation and escheat) and indirectly forecloses intergenerational transfer of clergy formation capacity from the communities to their own successors.
% ABSENT_VOICES: The Ecumenical Patriarchate's claimed ecumenical (not merely local) status, and the guarantor states' claimed supervisory role, are both raised repeatedly in diplomatic and ECtHR fora but are excluded from the domestic adjudicative process precisely because the restrictive reading defines them as non-justiciable domestic matters — the exclusion is the mechanism, not an oversight.
% DISAPPEARANCE_RATIONALE: If the restrictive reading disappeared and were replaced by the expansive or guarantor reading, confiscated foundation properties would become subject to restitution claims, Halki Seminary could reopen under a protected-institution theory, and Turkish domestic courts would face an entirely different evidentiary and jurisdictional posture in property and educational disputes — the state's asset base and administrative discretion over minority institutions would visibly contract.
% FOUNDING_PROBLEM: At Lausanne in 1923, Turkey needed a settlement that protected departing/remaining populations enough to secure great-power sign-off on the new Republic's borders and sovereignty, while the new state simultaneously pursued a nation-building project premised on legal and administrative uniformity that was in tension with continuing millet-style institutional autonomy for non-Muslim communities.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state (the benefiting party) attests the founding problem was narrowly about worship freedom and has been fully and durably resolved by current law. Outside corroboration is mixed and adverse to that view: the European Court of Human Rights (Fener Rum Erkek Lisesi Vakfı v. Turkey, 2007, and related property-restitution rulings), Council of Europe minority-rights reporting, and independent historians of the 1923 negotiations document that the treaty text and its negotiating history addressed institutional and property continuity, not worship alone — corroboration from outside the beneficiary state points toward the founding problem being still partly live and unresolved by the restrictive domestic reading.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at 2024) because the restrictive reading's operation transfers real, durable institutional assets (property, legal personality, clergy-formation capacity) from minority communities to the state with no compensating benefit flowing back to those communities — this is not a coordination cost, it is a one-directional transfer sustained by continuous legal and administrative enforcement. Suppression is high (0.78) because the reading's persistence depends on Turkish courts and administrative bodies actively refusing to treat institutional continuity, property, and education claims as within Lausanne's protective scope, foreclosing the primary legal avenue minority institutions would otherwise use. Theater ratio is moderate (0.42): the reading retains a genuine, non-performative core (general law does apply uniformly to all foundations and schools in form), but a rising share of enforcement activity over the interval (1971 seminary closure onward) function specifically to foreclose minority institutional continuity rather than to serve the neutral administrative purpose it is framed as. Accessibility collapse is high (0.72): once the domestic courts settled on the restrictive interpretation (especially post-1936, hardened by the 1970s), the practical alternative of asserting treaty-based institutional protection domestically essentially closed, leaving only international fora (ECtHR) as a partial, slow, non-binding-on-domestic-property-law avenue.
 *
 * PERSPECTIVAL GAP:
 *   From the Turkish state's seat, the restrictive reading looks like ordinary sovereign administration of civil society under a single general legal code — a rope, arguably, coordinating all foundations and schools under one regime. From the minority institutions' seat, the identical legal architecture operates as sustained extraction: assets and capacities that existed pre-1923 or were nominally guaranteed at Lausanne have been progressively transferred to the state through mechanisms (declaration requirements, general-law educational licensing) that formally apply neutrally but functionally target institutions with no domestic political constituency to resist them. This divergence is exactly what the engine's per-seat computation is built to surface — it is not resolved by picking a side in this story.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus and the Treasury foundations directorate sit at the full-beneficiary end: they administer the reading and the confiscated/escheated assets accrue directly to state and municipal balance sheets. The named minority communities sit at the full-target end: property, legal personality, and clergy-formation capacity flow away from them with no offsetting benefit, and their exit options range from trapped (Syriac communities, the seminary itself as a non-agent institution) to constrained (Armenian and Jewish communities, which can emigrate individually but cannot relocate institutional infrastructure). The Greek Orthodox Patriarchate is coded trapped rather than merely constrained because its seat (Istanbul) is doctrinally and canonically fixed — abandoning it is not a live option in the way individual emigration is for lay community members.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a 1923 great-power settlement balancing minority protection against Turkish nation-building sovereignty) is contested as to whether it is dead or live: the state treats it as fully and permanently resolved by current domestic law; independent corroboration (ECtHR rulings, Council of Europe reporting, negotiating-history scholarship) supports a live-and-unresolved reading. Because disappearance_verdict is world_rearranges (confiscated assets and institutional capacity would visibly change hands if the reading changed) while founding_problem_status is contested rather than cleanly dead, this does not present as a clean mandatrophy case (a mandate everyone agrees has outlived its function) — it presents as an active, still-contested extraction dressed in the settled language of domestic sovereignty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_scope_ambiguity,
    'Does Lausanne Article 40''s guarantee that minorities may ''establish, manage and control... any charitable, religious and social institutions'' extend treaty protection to institutional property and educational continuity, or only to the individual worship rights explicitly named in Article 38?',
    'Comparative treaty-interpretation analysis of the 1923 negotiating record (British and French delegation minutes), together with subsequent ECtHR jurisprudence interpreting analogous minority-protection clauses in other post-WWI settlements.',
    'If the treaty text is found to encompass institutional continuity, the restrictive reading is a misreading maintained by domestic power rather than a defensible textual interpretation, sharpening the snare classification; if the narrower reading is textually defensible, part of the measured extraction should instead be read as a genuine (if harsh) interpretive choice within treaty ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_scope_ambiguity, conceptual, 'Whether Lausanne''s text itself supports the restrictive reading''s narrow scope or the reading is an interpretive imposition.').

omega_variable(
    which_reading_is_the_kernel_default,
    'This story treats the restrictive reading as the reading actually enforced by Turkish domestic authority since 1923, but is that authority itself the legitimate adjudicator of Lausanne''s meaning, or does the guarantor_reading''s claim of international supervisory jurisdiction mean no single party''s reading is the kernel''s ''true'' state?',
    'Track ECtHR admissibility and merits rulings on Lausanne-adjacent minority property and institutional cases over time: an increasing rate of merits findings against Turkey would indicate international bodies do not treat the restrictive domestic reading as dispositive.',
    'If international bodies increasingly override the domestic reading, this constraint''s enforcement (requires_active_enforcement) becomes progressively less stable and the classification could drift from snare toward tangled_rope as external check-and-balance mechanisms partially restore contested exit options for minority institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_the_kernel_default, conceptual, 'Whether domestic adjudicatory authority or international supervisory authority is the operative reading-selector for this kernel, and how that affects enforcement stability.').

omega_variable(
    nation_building_versus_extraction_intent,
    'Was the restrictive reading originally adopted (1920s-30s) as a genuine, if harsh, legal-uniformity nation-building measure applicable to all civil institutions, or was it from inception designed specifically to dismantle minority institutional capacity?',
    'Archival research into the drafting history of the 1936 foundations declaration and comparison of its application to Muslim versus non-Muslim foundations in the same period.',
    'Evidence of differential application from inception would support treating the coordination function claimed above as largely pretextual (raising true extractiveness further); evidence of genuinely uniform early application with differential effect only emerging later would support a more mixed tangled_rope-adjacent reading of the early interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nation_building_versus_extraction_intent, empirical, 'Whether the restrictive legal architecture was designed for general uniformity or targeted dismantling of minority institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.2).
narrative_ontology:measurement(laus_tr_t1936, lausanne_minority_protections__restrictive_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__restrictive_reading, theater_ratio, 1971, 0.3).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__restrictive_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(laus_tr_t2008, lausanne_minority_protections__restrictive_reading, theater_ratio, 2008, 0.4).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(laus_be_t1936, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(laus_be_t2008, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.3).
narrative_ontology:measurement(laus_su_t1936, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1971, 0.68).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(laus_su_t2008, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the lausanne_minority_protections kernel, decomposed per the ε-invariance principle because the natural-language claim 'Lausanne minority protections' resolves to three structurally distinct claims with different epsilon values and different victim/beneficiary sets: the expansive_reading (Mountain-or-Rope-leaning: functional institutional continuity is treaty-guaranteed, minimal extraction claimed by the reading's own lights), the guarantor_reading (Tangled-Rope-leaning: internationally enforceable obligation, mixed coordination/extraction depending on enforcement gaps), and this restrictive_reading (Snare: high extraction, minority institutions as victim class, state apparatus as beneficiary). This reading forecloses the expansive_reading's core premise directly (both cannot be simultaneously true within Turkish domestic law: either institutional continuity is treaty-protected or it is a domestic matter subject to ordinary law) while merely influencing the guarantor_reading (the restrictive domestic posture raises the practical stakes and resource burden of pursuing the guarantor/ECtHR track without logically foreclosing its availability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
