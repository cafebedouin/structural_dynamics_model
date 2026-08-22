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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Treaty Minority Protections — Restrictive (Individual-Rights-Only) Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the restrictive reading of the Lausanne Treaty's
 *   minority protection provisions (Articles 37-45), one of three
 *   structurally distinct constraints the kernel supports. Under this
 *   reading, Lausanne guarantees only individual worship rights;
 *   institutional self-governance, property, and religious education are
 *   treated as ordinary domestic matters governed by general Turkish law,
 *   most concretely the Foundations Law and its 1936 declaration requirement,
 *   the 1971 nationalization of private higher education (which closed Halki
 *   Seminary), and subsequent Directorate General of Foundations
 *   administration. This reading has coincided with substantial documented
 *   losses: voided property titles for foundations that acquired real estate
 *   after 1936 without prior authorization, denial of legal personality to
 *   patriarchates, and closure of the sole domestic Orthodox seminary. The
 *   expansive reading (functional institutional continuity) and the guarantor
 *   reading (internationally supervised obligation) are separate constraints
 *   with different beneficiary/victim structures and different ε values —
 *   they are not alternate measurements of this one constraint but distinct
 *   constraints sharing a kernel text.
 *
 * KEY AGENTS:
 *   - turkish_state_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — sets the interpretive frame and captures administrative control of contested assets
 *   - directorate_general_of_foundations: agenda_setter/beneficiary (institutional/arbitrage) — administers the voiding and reversion mechanism directly
 *   - ecumenical_patriarchate: primary target (moderate/trapped) — denied legal personality, seminary closed, property exposure
 *   - minority_foundation_schools and non_muslim_religious_communities: diffuse targets (powerless/trapped-constrained) — bear institutional erosion and eventual loss of community infrastructure
 *   - guarantor_powers and european_court_of_human_rights: excluded analytical observers whose treaty or convention authority is not activated within this reading
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
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Treaty Minority Protections — Restrictive (Individual-Rights-Only) Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'c0e24260-2dcb-4584-b901-38043ce7333f').
narrative_ontology:cs_kernel_codification('c0e24260-2dcb-4584-b901-38043ce7333f', fixed_text).
narrative_ontology:cs_authority_grounding('c0e24260-2dcb-4584-b901-38043ce7333f', extraction).
narrative_ontology:cs_interpretation_layer_present('c0e24260-2dcb-4584-b901-38043ce7333f').
narrative_ontology:cs_reading_relation('c0e24260-2dcb-4584-b901-38043ce7333f', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('c0e24260-2dcb-4584-b901-38043ce7333f', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('c0e24260-2dcb-4584-b901-38043ce7333f', foundational, minority_protection_limited_to_individual_worship).
narrative_ontology:cs_axiom_status(minority_protection_limited_to_individual_worship, holdable).
narrative_ontology:cs_axiom_grounding('c0e24260-2dcb-4584-b901-38043ce7333f', minority_protection_limited_to_individual_worship, conventional).
narrative_ontology:cs_axiom('c0e24260-2dcb-4584-b901-38043ce7333f', foundational, domestic_sovereignty_governs_institutional_and_property_matters).
narrative_ontology:cs_axiom_status(domestic_sovereignty_governs_institutional_and_property_matters, holdable).
narrative_ontology:cs_axiom_grounding('c0e24260-2dcb-4584-b901-38043ce7333f', domestic_sovereignty_governs_institutional_and_property_matters, conventional).
narrative_ontology:cs_axiom('c0e24260-2dcb-4584-b901-38043ce7333f', secondary, equal_citizenship_precludes_special_communal_legal_status).
narrative_ontology:cs_axiom_status(equal_citizenship_precludes_special_communal_legal_status, holdable).
narrative_ontology:cs_axiom_grounding('c0e24260-2dcb-4584-b901-38043ce7333f', equal_citizenship_precludes_special_communal_legal_status, instrumental).
narrative_ontology:cs_reference_frame('c0e24260-2dcb-4584-b901-38043ce7333f', post_ottoman_secular_sovereignty_settlement).
narrative_ontology:cs_drift_state('c0e24260-2dcb-4584-b901-38043ce7333f', contemporary_eu_accession_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c0e24260-2dcb-4584-b901-38043ce7333f', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_patriarchate_of_istanbul).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, chief_rabbinate_of_turkey).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_foundation_schools).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Lausanne Article 40's 'equal right to establish, manage, and control' religious, charitable, and educational institutions as subject to ordinary domestic law rather than treaty-guaranteed autonomy. Administers the Foundations Law (2008 amendments notwithstanding) and Directorate General of Foundations oversight, retains discretion over legal personality recognition, property registration, and clergy training accreditation. Frames the arrangement as sovereign equal application of law to all citizens regardless of religion, and thereby captures administrative and territorial control over institutional assets that would otherwise sit outside state reach.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% The domestic administrative body that decides which minority foundation properties are validly held, which acquisitions since 1936 are void for lack of prior authorization, and which foundations may operate. Its rulings under this reading have resulted in confiscation or contested title over thousands of minority community properties; it collects assets into the state-administered foundation system when title is voided.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations, beneficiary).

% Denied legal personality as a religious institution under Turkish law; can act only through associated foundations. Cannot reopen the Halki Seminary (closed since 1971 under the 1971 nationalization of private higher education, upheld as domestically valid), meaning it cannot train its own clergy on Turkish soil under this reading. Cannot leave — its seat, historical properties, and claimed ecumenical status are fixed to Istanbul; relocation would dissolve the institution's claimed continuity.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate, payer,
    moderate, civilizational, trapped, national).

% Operates without recognized legal personality; its schools, hospitals, and cemetery foundations face the same domestic property-law exposure as other minority foundations, with a documented history of property loss and administrative obstruction. Community population decline compounds the institutional vulnerability — fewer members to contest state administrative decisions or fund litigation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_patriarchate_of_istanbul, payer,
    moderate, civilizational, trapped, national).

% A small and shrinking community whose religious foundations are subject to the same domestic foundations-law regime; theological training for rabbis must occur abroad since no protected right to domestic religious education exists under this reading. Faces the property-title and legal-personality constraints common to all non-Muslim foundations, with essentially no leverage to contest administrative rulings.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, chief_rabbinate_of_turkey, payer,
    powerless, generational, trapped, national).

% Greek, Armenian, and Jewish minority schools operate under Ministry of National Education curricular oversight rather than autonomous community control; deputy headmasters appointed by the ministry monitor compliance. Declining enrollment (driven by emigration and administrative friction) threatens school closures, which under domestic law can trigger property reversion. Cannot restructure their own governance to escape ministry oversight.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_foundation_schools, payer,
    powerless, biographical, trapped, national).

% The broader lay membership of Greek Orthodox, Armenian, Jewish, and Syriac communities who rely on these institutions for worship, education, and social services. As institutions lose property and administrative capacity, members lose access to community infrastructure; individual worship rights remain formally intact, but the institutional scaffolding around worship (schools, seminaries, endowed properties) erodes. Emigration is the primary exit, at the cost of leaving the historic community.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_religious_communities, payer,
    powerless, generational, constrained, national).

% The UK, France, and other original Lausanne signatories retain a treaty-law interest in Article 37-45 enforcement but exercise no active enforcement mechanism under this domestic-law reading; occasional diplomatic statements substitute for the treaty-supervision role the guarantor reading would assign them. Their potential objections (that domestic reinterpretation violates treaty obligations) are voiced in European Parliament resolutions and US State Department reports but carry no binding force under this reading.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_powers, excluded,
    institutional, generational, analytical, continental).

% Has ruled against Turkey in specific property cases (e.g., Fener Rum Erkek Lisesi Vakfı v. Turkey) under the European Convention rather than under Lausanne itself, producing case-by-case remedies without displacing the domestic-law framing of Lausanne. Its jurisprudence exists alongside, not within, this reading's kernel interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_court_of_human_rights, excluded,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Applies a single uniform property, foundation, and education law regime to all residents regardless of religion, avoiding a parallel or extraterritorial legal system for minority institutions and preserving the state's exclusive claim to define legal personality and institutional recognition within its territory.
% TRANSFER_FUNCTION: Moves institutional legal personality, property title, and clergy-training capacity from minority religious communities to the Turkish state and its foundation administration apparatus; over decades this has moved specific real property (churches, schools, cemeteries, seminaries) from community control into state-administered or third-party hands when title is voided or foundations are dissolved.
% ABSENT_VOICES: The Ecumenical Patriarchate, Armenian Patriarchate, and Chief Rabbinate would argue Lausanne's 'establish, manage, and control' language was drafted to guarantee functional institutional continuity, not merely to permit individual prayer; they raise this in diplomatic and legal channels but have no forum within Turkish domestic law that can override the restrictive reading. Guarantor powers and the European Court exist outside the domestic interpretive loop entirely.
% DISAPPEARANCE_RATIONALE: If the restrictive reading were abandoned in favor of institutional-autonomy recognition, minority foundations would regain contestable property claims, Halki Seminary could reopen, foundations could elect their own governance without ministry oversight, and decades of confiscation and dissolution rulings would become subject to challenge — a substantial reallocation of property and institutional authority back toward the minority communities.
% FOUNDING_PROBLEM: The 1923 Lausanne Treaty was built to settle post-Ottoman minority status by replacing the millet system's communal legal autonomy with treaty-guaranteed minority rights inside a new secular nation-state, preventing renewed great-power intervention on behalf of minorities while giving those minorities enforceable protections.
% FOUNDING_PROBLEM_CORROBORATION: Turkish state authorities attest the founding problem (great-power intervention pretexts) is resolved and that domestic law equality is the treaty's fulfillment. Independent corroboration from outside the benefiting state apparatus exists: European Court of Human Rights property rulings, US State Department International Religious Freedom reports, and academic treaty-law scholarship (e.g., work cataloguing post-1936 foundation property seizures) attest the founding problem of secure minority institutional continuity remains live and that the restrictive reading functions as ongoing institutional attrition rather than treaty fulfillment.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.81 at 2024) because the restrictive reading's operation has produced concrete, cataloged transfers of institutional capacity and property from minority communities to state administration — this is not a hypothetical cost but a documented historical pattern (1936 declarations, 1971 seminary closure, ongoing foundation property disputes). Suppression is high (0.78) because the reading depends on active administrative enforcement (title voiding, personality denial, curricular oversight) rather than voluntary participant acceptance; there is no consent mechanism by which the affected institutions chose this legal status. Theater ratio is moderate (0.42) reflecting that some genuine equal-treatment coordination function exists (a single property law regime avoids a fragmented millet-style parallel system) alongside performative equal-application rhetoric that masks asymmetric impact. Accessibility collapse is high (0.72): once the domestic-law interpretation is adopted, minority institutions have no domestic legal avenue to establish an alternative status — European Court remedies address specific violations without displacing the framework. Resistance is present but bounded (0.6): patriarchates and communities have pursued litigation and diplomatic channels, but structural trapped exit options limit resistance's effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus and Directorate General of Foundations sit at the beneficiary end: they set the interpretive rule, administer its application, and are the direct recipients of voided or reverted property and consolidated regulatory authority — d near 0. The patriarchates, rabbinate, and minority schools sit near the full-target end: trapped exit options (their historic seat, congregational base, and institutional continuity claims are fixed to specific Turkish territory), civilizational time horizons that make abandonment unthinkable as an identity matter, and direct exposure to property and personality loss — d near 1. Non-Muslim lay communities sit closer to symmetric-but-target: individual worship remains formally unimpaired (matching the reading's narrow guarantee), but the institutional infrastructure surrounding worship erodes, producing indirect but real costs; their exit option (emigration) exists but at high personal and communal cost, which is why exit_options is coded constrained rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive reading's founding-problem status is authored contested rather than dead, which is the analytically important move: the state's stated founding problem (preventing extraterritorial minority legal systems that invited great-power intervention) has arguably been solved — no current great power seriously threatens intervention on treaty grounds. But the reading persists and, on this authored account, has intensified (rising extractiveness and suppression through 2024) well past the point the original problem was resolved. This is the classic mandatrophy signature: an arrangement whose founding justification no longer requires this level of restriction, maintained because it now serves a different, undeclared function (administrative consolidation of minority institutional assets) rather than the original coordination problem. The corroboration field intentionally routes outside the benefiting state apparatus — ECtHR rulings and State Department reports — because a genealogy attested only by beneficiaries would be worthless as evidence of live vs. dead status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the Lausanne Treaty''s text and negotiating history support the restrictive (domestic-law-only) reading, or does the drafting record show the parties intended functional institutional continuity (the expansive reading) or continuing guarantor supervision (the guarantor reading)?',
    'Comparative textual and historical analysis of the 1923 Lausanne negotiating record, contemporaneous diplomatic correspondence among the guarantor powers, and comparison with how minority treaty provisions were interpreted and enforced in comparable interwar minority-protection treaties (e.g., Polish Minority Treaty) to establish whether ''establish, manage, and control'' was understood at the time as encompassing institutional autonomy.',
    'If the historical record supports the expansive reading, the restrictive reading''s classification as good-faith domestic sovereignty application would be substantially undermined, strengthening the treaty-violation characterization; if the record supports the restrictive reading, the current arrangement is a legitimate exercise of interpretive discretion within treaty bounds, which would push toward classifying this constraint closer to rope/tangled_rope territory rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the restrictive reading is the treaty''s originally intended meaning or a later narrowing.').

omega_variable(
    sovereignty_vs_extraction_framing,
    'Is the domestic-law framing a genuine, principled application of equal citizenship (no special legal status by religion) or a instrumentalized cover story that happens to concentrate institutional and property control in state hands?',
    'Compare enforcement patterns across minority groups and against majority (Sunni Muslim) religious institutions under the same general laws — if majority religious institutions face equivalent property/personality constraints under ''general law,'' the equal-citizenship framing gains support; if the general law regime as applied falls disproportionately or exclusively on non-Muslim minority institutions (e.g., the Presidency of Religious Affairs enjoys state funding and institutional support unavailable to minority foundations), the cover-story reading gains support.',
    'Disproportionate application would confirm the snare classification (coordination story as cover for asymmetric extraction); genuinely equal application across all religious institutions would support closer-to-rope characterization even under the restrictive textual reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_extraction_framing, empirical, 'Whether formal legal equality masks differential real-world impact by religious group.').

omega_variable(
    seat_selection_under_domain_ambiguity,
    'The obvious framing treats ''the Turkish state'' as a single agenda-setting seat; a less obvious framing separates the judiciary (which has occasionally ruled for minority foundations, e.g., partial 2011 property restitution decrees) from the executive/administrative Directorate General of Foundations (which retains discretionary control). Does this story''s single-state framing obscure internal institutional variation that would change the classification?',
    'Disaggregate Turkish state action into judicial, legislative (2008 Foundations Law amendments partially restoring some property rights), and administrative-executive strands, and assess whether any strand independently satisfies a rope or tangled_rope pattern even while the dominant administrative pattern remains snare-like.',
    'If disaggregated, part of the ''state apparatus'' seat might computably split into an agenda_setter seat showing partial rope characteristics (2008 restitution law) alongside a persistently snare-like administrative enforcement seat — this omega documents that choice was made in favor of the unified seat for this story, per the framing guidance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(seat_selection_under_domain_ambiguity, conceptual, 'Whether treating the Turkish state as one seat versus disaggregating judicial/legislative/administrative strands changes the cs_pattern classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.15).
narrative_ontology:measurement(laus_tr_t1936, lausanne_minority_protections__restrictive_reading, theater_ratio, 1936, 0.2).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__restrictive_reading, theater_ratio, 1971, 0.3).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__restrictive_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(laus_tr_t2008, lausanne_minority_protections__restrictive_reading, theater_ratio, 2008, 0.5).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(laus_be_t1936, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1990, 0.74).
narrative_ontology:measurement(laus_be_t2008, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.3).
narrative_ontology:measurement(laus_su_t1936, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1971, 0.65).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(laus_su_t2008, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the Lausanne Treaty minority-protection kernel (kernel_id: lausanne_minority_protections). The restrictive_reading (this file) authors high extractiveness with minority institutions as victims and the state apparatus as beneficiary. The expansive_reading authors the same treaty text as guaranteeing functional institutional continuity — under that reading the historical property confiscations and seminary closure are authored as treaty breaches, inverting much of the beneficiary/victim structure. The guarantor_reading authors Lausanne as an internationally supervised obligation enforceable through guarantor-state diplomacy and ECHR mechanisms rather than domestic interpretation alone — under that reading the guarantor_powers and european_court_of_human_rights stakeholders (excluded here) hold active enforcement roles. All three share the fixed 1923 treaty text as kernel; they diverge entirely on authority_grounding and axiom content. Per the ε-invariance principle, these are three separate files with three separate ε values, linked here rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
