% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Contextual-Egalitarian (Maqasid) Reading of Qur'anic Gender Verses
 *   domain: Islamic Jurisprudence / Legal Hermeneutics / Gender Studies
 *
 * SUMMARY:
 *   This story authors the contextual-egalitarian reading of a contested
 *   kernel: the correct interpretive status of Qur'anic verses governing
 *   gender-differentiated inheritance (4:11), testimony (2:282), and
 *   guardianship (4:34). This reading treats the verses as historically
 *   situated progressive reforms relative to pre-Islamic Arabian norms, whose
 *   substantive application today should be governed by the Qur'an's own
 *   overarching equity objectives (maqasid al-shari'ah) rather than by the
 *   literal 7th-century allocation. As this reading gains institutional
 *   traction (adoption by reform-oriented scholars, codification pressure on
 *   state family courts, uptake by rights NGOs), interpretive authority and
 *   some material entitlements shift away from literalist seminaries,
 *   traditional qadis, and patriarchal family heads toward reformist jurists,
 *   advocacy organizations, and women litigants. Two sibling readings of the
 *   same kernel — literal_hierarchical and progressive_abrogation — are NOT
 *   part of this story; they are separate constraints with their own epsilon
 *   and stakeholder structures, linked here only via network edges and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - reformist_scholars: agenda_setter (organized/mobile) — develop and promulgate the maqasid reading, gain interpretive authority
 *   - womens_rights_ngos: beneficiary/agenda_setter (organized/mobile) — gain legal leverage and legitimated standing
 *   - muslim_women_seeking_equal_inheritance: beneficiary (moderate/constrained) — gain material claims to equal shares
 *   - traditional_qadis_losing_discretionary_authority: payer (institutional/constrained) — lose discretionary interpretive power
 *   - patriarchal_family_heads: payer (moderate/constrained) — lose guardianship and inheritance-share prerogatives
 *   - literalist_seminaries: payer/excluded (institutional/identity_locked) — institutional identity threatened by delegitimization
 *   - state_family_courts: agenda_setter/observer (institutional/constrained) — decide which reading to codify
 *   - ordinary_believers_outside_dispute: excluded (powerless/trapped) — live under whichever reading wins, no voice in the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.42).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.38).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.42).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Contextual-Egalitarian (Maqasid) Reading of Qur'anic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "Islamic Jurisprudence / Legal Hermeneutics / Gender Studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '9bbdee72-2678-4cc4-be5a-d7077e9eead0').
narrative_ontology:cs_kernel_codification('9bbdee72-2678-4cc4-be5a-d7077e9eead0', fixed_text).
narrative_ontology:cs_authority_grounding('9bbdee72-2678-4cc4-be5a-d7077e9eead0', practice).
narrative_ontology:cs_interpretation_layer_present('9bbdee72-2678-4cc4-be5a-d7077e9eead0').
narrative_ontology:cs_reading_relation('9bbdee72-2678-4cc4-be5a-d7077e9eead0', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('9bbdee72-2678-4cc4-be5a-d7077e9eead0', quranic_gender_verses__progressive_abrogation, influences).
narrative_ontology:cs_axiom('9bbdee72-2678-4cc4-be5a-d7077e9eead0', foundational, verses_are_historically_situated_not_timeless_legislation).
narrative_ontology:cs_axiom_status(verses_are_historically_situated_not_timeless_legislation, holdable).
narrative_ontology:cs_axiom_grounding('9bbdee72-2678-4cc4-be5a-d7077e9eead0', verses_are_historically_situated_not_timeless_legislation, conventional).
narrative_ontology:cs_axiom('9bbdee72-2678-4cc4-be5a-d7077e9eead0', foundational, maqasid_equity_principles_govern_substantive_application).
narrative_ontology:cs_axiom_status(maqasid_equity_principles_govern_substantive_application, holdable).
narrative_ontology:cs_axiom_grounding('9bbdee72-2678-4cc4-be5a-d7077e9eead0', maqasid_equity_principles_govern_substantive_application, instrumental).
narrative_ontology:cs_reference_frame('9bbdee72-2678-4cc4-be5a-d7077e9eead0', classical_literalist_ahkam_application).
narrative_ontology:cs_drift_state('9bbdee72-2678-4cc4-be5a-d7077e9eead0', post_colonial_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9bbdee72-2678-4cc4-be5a-d7077e9eead0', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, muslim_women_seeking_equal_inheritance).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, muslim_women_seeking_equal_testimony_standing).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_qadis_losing_discretionary_authority).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_family_heads).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, literalist_seminaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and promulgate the maqasid-based reinterpretation, arguing that verses like 4:11 (inheritance) and 4:34 (guardianship) encode 7th-century Arabian social conditions rather than timeless ordinance, and that the Qur'an's overarching equity objectives should govern application today. They publish fatwas, train jurists, and lobby for legislative codification, gaining interpretive authority and institutional standing as their reading is adopted.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    organized, generational, mobile, global).

% Use the contextual-egalitarian reading as legal and rhetorical ammunition in campaigns for equal inheritance and testimony law reform. Gain funding, standing, and coalition access when the reading is credentialed as authentically Islamic rather than a foreign imposition; their institutional relevance is partly built on this reading succeeding.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, womens_rights_ngos, agenda_setter).

% Stand to gain direct material claims—equal inheritance shares, testimony weighted equally to men's—if courts adopt this reading. Currently their exit from unfavorable rulings is limited to appeal within religious courts or migration to secular jurisdictions; this reading changes what a favorable outcome looks like without requiring exit.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, muslim_women_seeking_equal_inheritance, beneficiary,
    moderate, biographical, constrained, national).

% Have built careers and courtroom authority on literal application of the classical rulings. If the maqasid reading is adopted by state legal systems, their interpretive discretion narrows, their rulings become subject to appellate reversal on egalitarian grounds, and their status as guardians of settled doctrine erodes. They cannot easily retrain into a different jurisprudential authority late in career.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_qadis_losing_discretionary_authority, payer,
    institutional, generational, constrained, national).

% Currently hold guardianship prerogatives and larger inheritance shares justified by the literal reading. Adoption of the contextual reading directly reduces their household economic and legal authority; their exit options are limited to relocating to jurisdictions still applying the literal standard or resisting reform politically.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_family_heads, payer,
    moderate, biographical, constrained, local).

% Their institutional identity and centuries of accumulated legal scholarship are built on the premise that these verses are direct, timeless divine legislation. The contextual reading, if it becomes dominant, delegitimizes their curriculum and reduces their claim to sole interpretive authority. Exit is not really available—their institutional identity IS the literal reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, literalist_seminaries, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, literalist_seminaries, excluded).

% Decide which jurisprudential reading to codify into family law and apply in disputes over inheritance, divorce, and testimony. Face political pressure from all sides and must weigh legitimacy costs of appearing either too secular or too regressive; their choice of reading reallocates rights and authority across the other seats.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, state_family_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, state_family_courts, observer).

% Most practicing Muslims are not party to elite hermeneutical disputes but live under whichever reading their local courts and communities adopt. They have no direct voice in which scholarly camp prevails, yet their marriage, inheritance, and testimony outcomes are set by the winning reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, ordinary_believers_outside_dispute, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical mechanism for applying a text formed in a specific historical moment to changed social conditions without declaring the text obsolete—preserving textual authority while updating substantive outcomes via appeal to the Qur'an's own stated higher objectives (maqasid).
% TRANSFER_FUNCTION: Moves interpretive authority from literalist seminaries and traditional qadis toward reformist scholars and allied rights organizations; moves material legal entitlements (inheritance share, testimony weight, guardianship discretion) from patriarchal family heads toward women who benefit from egalitarian application.
% ABSENT_VOICES: Ordinary believers who live under whichever ruling prevails have no seat in the scholarly contest; rural and lower-income women who cannot access reform-oriented courts or NGO advocacy are affected by the outcome but not represented in the debate that decides it.
% DISAPPEARANCE_RATIONALE: If the contextual-egalitarian reading were withdrawn entirely from legal and scholarly circulation, reformist scholars would lose their primary jurisprudential lever, rights NGOs would lose a locally-legitimated argument and likely pivot to purely secular/international-law framing, and courts currently applying maqasid-based rulings on inheritance and testimony would revert to literalist defaults—materially changing outcomes for the women currently benefiting from it.
% FOUNDING_PROBLEM: Classical jurists in the 7th–9th century applied Qur'anic verses on inheritance, testimony, and guardianship as direct legislation for their social context; by the 20th–21st century, changed economic and social conditions for women (education, wage labor, single-headed households) created a gap between literal application and the Qur'an's own stated commitments to justice and equity, which the maqasid framework was developed to close.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars (Fazlur Rahman's school, Amina Wadud, Khaled Abou El Fadl) attest the gap is real and growing. Independent corroboration comes from comparative family-law scholars and UN CEDAW committee reviews documenting material disparities in inheritance and testimony outcomes across jurisdictions applying different readings—evidence produced outside the reformist camp itself. Literalist seminaries dispute that any gap exists, holding the classical application to be itself the equity standard; this dispute is the contest the kernel names.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).
:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 rather than low or high: this reading does genuinely redistribute concrete legal entitlements (inheritance shares, testimony weight, guardianship discretion) away from an incumbent group (patriarchal family heads, traditional qadis) toward beneficiaries (women, reformist institutions) — that is real transfer, not mere reframing, which rules out a pure-rope reading. But the mechanism operates through persuasion, scholarship, and legislative codification rather than through coercive suppression of alternative textual readings — literalist scholarship continues to be produced, taught, and legally operative in many jurisdictions, which keeps extractiveness well below snare territory. Suppression starts moderate (0.50) reflecting early-period marginalization of reformist voices by established religious authorities, and DECLINES over the interval (to 0.38) as the reading gains institutional footholds, journals, and state adoption — the opposite trajectory from a hardening enforcement regime. Resistance is authored high (0.72) because literalist seminaries and traditional qadis mount sustained, well-resourced doctrinal and political opposition; this is not a quietly-accepted reform. Theater ratio is low-to-moderate (0.22) and drifts upward slightly as some jurisdictions adopt maqasid language in family-law codes without materially changing enforcement outcomes (symbolic reform without full substantive shift).
 *
 * PERSPECTIVAL GAP:
 *   From the reformist_scholars/womens_rights_ngos seat, this reading is emancipatory coordination: it resolves a genuine textual-application problem (how to honor divine text under radically changed social conditions) using the Qur'an's own internal equity logic. From the traditional_qadis and literalist_seminaries seat, the same reading operates as extraction of their interpretive authority and delegitimization of their life's scholarly work — a transfer dressed as hermeneutics. The engine computes these as structurally different seat classifications from the same authored data; neither seat is in error, and the divergence itself is the analytical finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and NGOs sit near the beneficiary end: they gain authority, funding, and legitimacy as the reading spreads, with mobile exit options (they can operate across jurisdictions and are not dependent on any single court's ruling). Muslim women seeking equal inheritance/testimony are beneficiaries but with constrained exit — they cannot simply relocate to more favorable courts and must work through whatever system currently governs their case. Traditional qadis and patriarchal family heads sit near the target end: they bear a direct loss of discretionary authority or material entitlement, and their exit options are constrained by career and social position. Literalist seminaries are identity-locked — their institutional existence is constituted by defending the literal reading, making this the paradigm case of institutional identity-fusion rather than mere interest-based opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not present as mandatrophic on its own terms: its proponents argue the founding problem (applying revealed text under changed social conditions consistent with the text's own equity commitments) is very much alive, not a dead mandate being defended by inertia. However, the founding_problem_status is authored as contested rather than settled — literalist seminaries dispute that any interpretive gap exists at all, holding classical application to already BE the equity standard. Classifying this as tangled_rope rather than snare or pure rope avoids two mislabeling failures: treating it as pure extraction would ignore the genuine coordination function it performs (giving textually-grounded language to real material claims that would otherwise require abandoning religious framing entirely), while treating it as pure rope would ignore that its adoption does concretely transfer discretionary authority and material entitlements away from an identifiable incumbent group who did not consent to the reinterpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maqasid_authority_source_ambiguity,
    'Is the maqasid framework itself a genuine classical interpretive tool with deep roots in usul al-fiqh (as reformists claim, citing al-Shatibi and al-Ghazali), or is it a modern importation that retrofits contemporary human-rights language onto classical vocabulary to gain religious legitimacy for an externally-derived egalitarian commitment?',
    'Historical-textual analysis tracing the actual scope of maqasid usage in classical jurisprudence (was it ever applied to override explicit textual rulings, or only to fill gaps where no clear verse existed?) compared against its scope of application in 20th/21st century reformist scholarship.',
    'If maqasid was classically restricted to gap-filling and never used to override explicit ahkam verses, the contextual_egalitarian reading''s claim to methodological continuity with classical tradition weakens substantially, and its extraction from literalist authority looks more like innovation dressed in classical clothing. If classical precedent for maqasid-overriding-explicit-text exists, the reading''s legitimacy claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maqasid_authority_source_ambiguity, conceptual, 'Whether maqasid-based override of explicit verses has genuine classical precedent or is a modern methodological innovation.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does this reading''s disagreement with literal_hierarchical and progressive_abrogation live — is it a disagreement about WHAT THE TEXT MEANS (hermeneutics), about WHOSE AUTHORITY DECIDES meaning (institutional/political), or about WHAT OUTCOME IS DESIRED (values), with hermeneutics recruited post hoc?',
    'Track whether individual scholars'' positions on maqasid-authority shift when the substantive policy outcome is held constant vs. varied — if reformist scholars who apply maqasid to override 4:11 do NOT apply equivalent maqasid reasoning to override verses that would produce outcomes they find undesirable, this is evidence of values-first reasoning; consistent application across cases favoring and disfavoring reform outcomes would support genuine hermeneutic-first reasoning.',
    'If values-first, the contextual_egalitarian reading''s claimed methodological neutrality is undermined and it looks structurally similar to progressive_abrogation (both would be outcome-driven with different textual justification apparatus). If hermeneutics-first, the reading has a stronger claim to being a genuinely distinct interpretive tradition rather than a rhetorical variant of the abrogation reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the contest among sibling readings is primarily hermeneutical, institutional, or values-driven, with implications for how cleanly the readings are actually distinguishable.').

omega_variable(
    state_adoption_causal_direction,
    'When state family courts adopt maqasid-based rulings, is this reading DRIVING legal reform, or is it being adopted AS COVER for reforms that state actors want for independent political reasons (international pressure, economic modernization, CEDAW compliance obligations)?',
    'Compare timing and sequencing of maqasid-reasoning adoption in court rulings against independent political/diplomatic pressure events (CEDAW reporting cycles, foreign aid conditionality, domestic secular feminist mobilization) in specific jurisdictions.',
    'If state adoption is primarily instrumental (using religious language to legitimate externally-pressured reform), the reading''s real causal weight in producing outcomes is smaller than its rhetorical prominence suggests, and the beneficiary/victim structure authored here may overstate the reading''s own causal contribution relative to other pressures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_adoption_causal_direction, empirical, 'Whether state legal adoption of this reading is hermeneutically driven or instrumentally adopted to legitimate externally-pressured reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__contextual_egalitarian, theater_ratio, 8, 0.15).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__contextual_egalitarian, theater_ratio, 16, 0.17).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__contextual_egalitarian, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__contextual_egalitarian, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__contextual_egalitarian, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 32, 0.39).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, progressive_abrogation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quranic_gender_verses kernel. literal_hierarchical treats the same verses as direct timeless legislation (near-mountain from a traditionalist seat; low authored extraction, high accessibility_collapse). progressive_abrogation treats the verses as an incomplete trajectory superseded by later universalist verses via naskh (distinct textual mechanism, likely produces a different beneficiary/victim proportion given its more categorical supersession claim vs. this reading's contextual-recalibration claim). All three share victim/beneficiary population overlap (traditional authorities lose, reform-aligned women's claims gain) but differ in claimed_type, epsilon, and the specific mechanism (contextualization vs. abrogation vs. literal-timeless) by which the outcome is reached. Each carries its own epsilon per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
