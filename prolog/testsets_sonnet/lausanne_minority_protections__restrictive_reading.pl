% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Restrictive (Individual-Worship-Only) Reading of Lausanne Minority Protections
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the restrictive reading of the Lausanne Treaty's
 *   minority protection regime (Article 37-45): the position that Article
 *   40's guarantee of religious and educational rights extends only to
 *   individual freedom of worship and belief, while institutional autonomy,
 *   property ownership, and theological education for minority communities
 *   (Greek Orthodox, Armenian Apostolic, Jewish) are ordinary domestic
 *   matters governed by general Turkish law with no special treaty status.
 *   Under this reading, the 1936 Declarations regime, the closure of the
 *   Halki Theological School since 1971, and periodic property confiscations
 *   under general foundations law are lawful exercises of domestic
 *   sovereignty rather than treaty violations. This is a snare: the
 *   coordination story (uniform domestic law, no parallel legal order) is
 *   real as far as it goes, but the same structure that achieves legal
 *   uniformity also strips protected communities of the institutional
 *   capacity the pre-1923 order recognized, with no domestic forum to contest
 *   the underlying classification. The sibling readings — expansive_reading
 *   (institutional continuity is guaranteed) and guarantor_reading
 *   (protections are internationally supervised, not solely domestic) — are
 *   separate constraint stories with their own ε values; per the ε-invariance
 *   principle, this story does not average across them or hedge its own
 *   extractiveness to accommodate them.
 *
 * KEY AGENTS:
 *   - turkish_state_apparatus: agenda_setter (institutional/analytical) — administers and enforces the restrictive interpretation
 *   - directorate_general_of_foundations: beneficiary/agenda_setter (institutional/analytical) — accumulates administrative and title control over minority assets
 *   - ecumenical_patriarchate: payer (powerless/trapped) — denied legal personality, clergy training foreclosed since 1971
 *   - armenian_patriarchate_institutions, jewish_community_foundations, minority_theological_seminaries, minority_foundation_property_holders: payers bearing property and institutional loss
 *   - guarantor_powers, council_of_europe_human_rights_mechanisms: excluded — treaty-diplomatic and supra-domestic channels sidelined by the domestic-matter framing
 *   - international_legal_scholars: observer — document the negotiating-history gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.81).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.72).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Restrictive (Individual-Worship-Only) Reading of Lausanne Minority Protections").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '42afd730-1c66-4281-8df9-453682d8d1c8').
narrative_ontology:cs_kernel_codification('42afd730-1c66-4281-8df9-453682d8d1c8', fixed_text).
narrative_ontology:cs_authority_grounding('42afd730-1c66-4281-8df9-453682d8d1c8', extraction).
narrative_ontology:cs_interpretation_layer_present('42afd730-1c66-4281-8df9-453682d8d1c8').
narrative_ontology:cs_reading_relation('42afd730-1c66-4281-8df9-453682d8d1c8', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('42afd730-1c66-4281-8df9-453682d8d1c8', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('42afd730-1c66-4281-8df9-453682d8d1c8', foundational, treaty_protection_limited_to_individual_conscience).
narrative_ontology:cs_axiom_status(treaty_protection_limited_to_individual_conscience, holdable).
narrative_ontology:cs_axiom_grounding('42afd730-1c66-4281-8df9-453682d8d1c8', treaty_protection_limited_to_individual_conscience, conventional).
narrative_ontology:cs_axiom('42afd730-1c66-4281-8df9-453682d8d1c8', foundational, unitary_domestic_law_supersedes_parallel_institutional_jurisdiction).
narrative_ontology:cs_axiom_status(unitary_domestic_law_supersedes_parallel_institutional_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('42afd730-1c66-4281-8df9-453682d8d1c8', unitary_domestic_law_supersedes_parallel_institutional_jurisdiction, instrumental).
narrative_ontology:cs_reference_frame('42afd730-1c66-4281-8df9-453682d8d1c8', unitary_republican_legal_sovereignty).
narrative_ontology:cs_drift_state('42afd730-1c66-4281-8df9-453682d8d1c8', contemporary_echr_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('42afd730-1c66-4281-8df9-453682d8d1c8', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_patriarchate_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, jewish_community_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_theological_seminaries).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_foundation_property_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Lausanne Article 40 as guaranteeing only individual freedom of worship, not institutional continuity. Applies general Turkish law (Foundations Law, Treasury seizure statutes, Higher Education Board authority) to minority religious bodies exactly as it would to any domestic association, denying them the sui generis legal personality the pre-1923 millet system afforded. Administers foundation registries, appoints trustees where legal personality lapses, and closes institutions that cannot re-register under domestic categories that do not fit their religious character.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Administers the 1936 Declarations regime and subsequent foundation law, through which minority community properties acquired after 1936 (or not listed in the original declaration) can be seized as escheated or improperly held. Accumulates administrative control and, in many documented cases, direct title over former minority-held real estate, orphanages, hospitals, and cemeteries. Bears none of the loss when institutions close.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations, agenda_setter).

% Denied legal personality under this reading, forcing it to hold property through proxy foundations vulnerable to state reassignment. The Halki Theological School has remained closed since 1971 because clergy training is classified as private education subject to state licensing that is not granted. Cannot train its own clergy, cannot freely elect successors recognized as having civil legal standing, and has no domestic forum in which to contest the classification itself since the classification is the thing at issue.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate, payer,
    powerless, civilizational, trapped, national).

% Community foundations have had scores of properties confiscated or contested under the 1936 Declarations rule; schools and hospitals face registration and trustee-appointment risk under domestic association law. Community leaders can appeal individual seizures through Turkish courts but cannot challenge the underlying restrictive interpretation, since the interpretation is treated as settled domestic constitutional doctrine rather than a live treaty question.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_patriarchate_institutions, payer,
    powerless, generational, constrained, national).

% Smaller and more assimilated than the Christian minorities but subject to the same foundations-law exposure: community trusts must operate as ordinary Turkish foundations, with property and governance decisions reviewable by the same Directorate. Emigration has reduced the community's numbers, further weakening any coalition capacity to contest the restrictive reading.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, jewish_community_foundations, payer,
    powerless, generational, constrained, national).

% Classified as private educational institutions under the Higher Education Board's general licensing regime rather than as protected religious training bodies. Licensing is not granted, so clergy formation for the affected communities happens abroad if at all, producing a structural succession crisis measured in decades. There is no exit: the institution cannot relocate its licensing dependency and cannot train alternatives domestically.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_theological_seminaries, payer,
    powerless, civilizational, trapped, national).

% Hold title (where they still can) to churches, synagogues, schools, and cemeteries under domestic foundation law rather than protected minority-institution status. Property acquired or improved after the 1936 declaration cutoff, or held informally due to historical restrictions on minority ownership, is exposed to state reassignment. Restitution processes exist but are slow, discretionary, and do not address the underlying classification that produced the exposure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_foundation_property_holders, payer,
    powerless, generational, trapped, national).

% Britain, France, and the other Lausanne signatories retain a treaty-law argument for diplomatic engagement on minority protection but rarely exercise it forcefully; under the restrictive reading, the matter is characterized as internal to Turkish domestic law, which forecloses the diplomatic-obligation channel entirely. Their potential objection is real but structurally sidelined by the reading itself.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_powers, excluded,
    institutional, generational, constrained, continental).

% The European Court of Human Rights has ruled against Turkey in specific property and legal-personality cases (e.g. Fener Rum Erkek Lisesi), but these rulings operate as individual remedies under ECHR property and religion articles, not as a reversal of the restrictive Lausanne interpretation itself. The restrictive reading's domestic-matter framing keeps the treaty question itself outside the Court's Lausanne jurisdiction.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, council_of_europe_human_rights_mechanisms, excluded,
    institutional, generational, constrained, continental).

% Document the gap between the 1923 negotiating history (which discussed institutional continuity) and the restrictive reading's individual-worship-only construction, and track case outcomes across the three competing readings without power to bind any party to a resolution.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Turkey a single, internally consistent legal doctrine for applying uniform domestic law to all religious and civic associations, avoiding a parallel legal order for historic minorities that could be read as extraterritorial or as undermining post-1923 nation-state consolidation.
% TRANSFER_FUNCTION: Moves institutional legal personality, property title, and clergy-training capacity from minority religious communities to the Turkish state and its foundation administration apparatus, converting what the pre-1923 millet system treated as autonomous community assets into assets subject to ordinary discretionary state administration.
% ABSENT_VOICES: The 1923 Lausanne negotiators' own recorded intent regarding institutional continuity is not consulted as binding; guarantor powers who could raise the treaty question diplomatically largely decline to; the communities themselves have no domestic forum to contest the classification because the classification determines what counts as a justiciable claim in the first place.
% DISAPPEARANCE_RATIONALE: If the restrictive reading were abandoned in favor of the expansive reading, Halki would reopen, confiscated 1936-era properties would face a fresh restitution claim, minority foundations would regain sui generis legal personality, and the Directorate General of Foundations would lose a substantial category of administered and absorbed assets — a materially different institutional landscape, not a cosmetic one.
% FOUNDING_PROBLEM: Post-1923 Turkey needed a legal framework that protected genuine religious liberty for minorities while dismantling the Ottoman millet system's parallel legal jurisdictions, seen as incompatible with unitary republican sovereignty and secular legal uniformity.
% FOUNDING_PROBLEM_CORROBORATION: Turkish constitutional and administrative law scholars within the state tradition attest the unitary-sovereignty problem remains live and justifies the restrictive reading. Outside corroboration is thin and points the other way: European Court of Human Rights rulings, Council of Europe minority-rights reporting, and independent international-law historians (using Lausanne's own negotiating record) attest that the sovereignty-consolidation problem was substantially resolved by the 1930s and that the restrictive reading's persistence since then serves asset and control consolidation rather than the original unitary-law concern.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.81) is high and rising across the measured interval because each subsequent legal instrument (1936 Declarations, Higher Education Board licensing regime, foundation-law amendments) narrows minority institutional capacity further while the treaty-interpretation basis remains constant — this is rent accumulation on a fixed doctrinal foundation, not a one-time transfer. Suppression (0.72) reflects that persistence requires active administrative and sometimes judicial enforcement (foundation seizures, licensing denials) rather than passive drift; it dipped slightly around 1990 reflecting a period of comparatively reduced new seizures before renewed foundation-law activity resumed the climb. Theater ratio (0.44) is moderate: general-law uniformity is a genuine governance function for the broader Turkish legal system, but an increasing share of enforcement specifically targets minority institutional capacity rather than serving neutral administrative ends. Accessibility collapse (0.62) is substantial but not absolute — ECHR litigation remains a partial alternative channel, which is why it is not scored near mountain-level. Resistance (0.58) reflects sustained but resource-constrained community advocacy, diaspora lobbying, and periodic ECHR victories, without power to alter the underlying doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus and the Directorate General of Foundations sit at the beneficiary end: they set the interpretation, administer its consequences, and in the Directorate's case directly gain title and control over reassigned assets. The five payer groups are trapped or constrained: legal personality denial and licensing foreclosure are not costs they can price or route around, since the classification itself removes the forum in which they could contest it. Guarantor powers and Council of Europe mechanisms are excluded rather than positioned as payers or beneficiaries — their institutional interest in enforcement is real but structurally sidelined by the domestic-matter characterization, which is exactly the doctrinal move that forecloses their channel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dismantling parallel millet-system jurisdictions to secure unitary republican sovereignty) was substantially resolved by the 1930s once the legal architecture of the secular republic was established; the restrictive reading's continued application since then increasingly serves asset consolidation and institutional control rather than the original sovereignty concern. This is the mismatch the six-questions genealogy is designed to surface: founding_problem_status is coded contested rather than flatly dead because the state tradition still asserts the problem is live, but outside corroboration (ECHR rulings, independent minority-rights scholarship) points toward a founding problem that has been substantially solved for decades while the restrictive doctrine persists and continues extracting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restrictive_reading_negotiating_history_fit,
    'Does the 1923 Lausanne Conference negotiating record support the restrictive (individual-worship-only) construction of Article 40, or did the drafters intend institutional continuity that the restrictive reading has since narrowed away?',
    'Systematic review of the Lausanne Conference minutes, League of Nations correspondence on minority treaties, and comparative analysis against other interwar minority treaties (e.g., the Polish Minority Treaty) that used more explicit institutional-protection language.',
    'If the historical record shows the drafters intended institutional continuity, the restrictive reading is a later doctrinal narrowing rather than the treaty''s original meaning — strengthening the case that this constraint is a constructed extraction dressed as fidelity to the text, not a natural reading of it. If the record is genuinely ambiguous, the restrictive reading retains a stronger claim to being one legitimate interpretation among several rather than a doctrinal capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restrictive_reading_negotiating_history_fit, empirical, 'Whether the restrictive reading matches or narrows the original 1923 negotiating intent.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Why does Turkish domestic jurisprudence consistently select the restrictive reading over the expansive or guarantor readings, when the treaty text itself does not compel this choice among the three live interpretations?',
    'Trace the doctrinal lineage of the restrictive reading through Turkish Constitutional Court and Council of State jurisprudence, identifying whether the selection is driven by textual analysis, by a background unitary-sovereignty doctrine treated as constitutionally prior to treaty interpretation, or by administrative convenience for the Directorate General of Foundations.',
    'If selection is driven primarily by a prior sovereignty doctrine or administrative convenience rather than textual analysis, this supports classifying the restrictive reading''s persistence as extraction-serving doctrine rather than good-faith interpretation — reinforcing the snare classification over a genuine-disagreement account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether the choice among the three kernel readings is textually driven or interest-driven.').

omega_variable(
    echr_channel_sufficiency,
    'Does the availability of individual ECHR remedies (property article, religion article) for specific confiscation and licensing cases substantively compensate for the restrictive reading''s foreclosure of the general treaty-interpretation question, or is case-by-case litigation structurally insufficient to address a systemic doctrinal foreclosure?',
    'Comparative analysis of ECHR case outcomes and enforcement (compliance with judgments, rate of resolution) against the scale of unresolved property and legal-personality claims across all affected minority institutions.',
    'If ECHR remedies are structurally insufficient at scale, the accessibility_collapse score (0.62) may understate the true collapse of alternatives, since the remaining channel is illusory relief rather than a genuine alternative path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(echr_channel_sufficiency, empirical, 'Whether individual ECHR litigation is a real alternative to the foreclosed treaty channel.').


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
narrative_ontology:measurement(laus_tr_t1955, lausanne_minority_protections__restrictive_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__restrictive_reading, theater_ratio, 1971, 0.34).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__restrictive_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(laus_tr_t2010, lausanne_minority_protections__restrictive_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.38).
narrative_ontology:measurement(laus_be_t1936, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1936, 0.52).
narrative_ontology:measurement(laus_be_t1955, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1955, 0.68).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1971, 0.74).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1990, 0.77).
narrative_ontology:measurement(laus_be_t2010, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.3).
narrative_ontology:measurement(laus_su_t1936, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(laus_su_t1955, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1955, 0.63).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1971, 0.69).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(laus_su_t2010, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This story is one of three ε-invariant siblings decomposing the natural-language concept 'Lausanne minority protections' per the ε-invariance principle: restrictive_reading (this story, high-extraction snare), expansive_reading (institutional-continuity guarantee, coordination-favorable), and guarantor_reading (internationally supervised obligation, shifts enforcement locus outward). Each carries its own stable ε, beneficiary/victim structure, and classification rather than a single story hedging across interpretations. The restrictive reading forecloses the expansive reading's core premise within any single interpreting framework, and exerts downstream influence on the guarantor reading by suppressing (without logically eliminating) the practical activation of the diplomatic/supranational enforcement channel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
