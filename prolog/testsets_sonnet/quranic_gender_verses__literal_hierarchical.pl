% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Literal-Hierarchical Reading of Qur'anic Gender Verses (4:11, 2:282, 4:34)
 *   domain: religious_legal/gender
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Qur'anic gender-verses kernel
 *   (4:11, 2:282, 4:34): the literal-hierarchical reading, which treats these
 *   verses as direct, timeless legal ordinance establishing male
 *   guardianship, a fixed 2:1 inheritance ratio favoring male heirs, and
 *   testimony weighted by sex. This is not a claim about what the Qur'an
 *   means overall — it is a claim about the structural consequences of ONE
 *   textually-committed reading as it is institutionally codified in
 *   religious family courts. The sibling readings (contextual_egalitarian,
 *   progressive_abrogation) are separate constraints with their own ε values,
 *   beneficiary/victim structures, and network links; they are not blended
 *   into this file.
 *
 * KEY AGENTS:
 *   - male_household_heads: Primary beneficiary (powerful/arbitrage) — collects guardianship authority and disciplinary standing
 *   - religious_court_authorities: Institutional agenda_setter (institutional/analytical) — administers and enforces the literal codification
 *   - male_heirs: Passive beneficiary (moderate/mobile) — receives inheritance advantage without needing to advocate for it
 *   - women_heirs: Primary target (powerless/trapped) — bears the halved inheritance share
 *   - female_witnesses: Target (powerless/trapped) — bears halved testimonial weight
 *   - wives_under_guardianship: Target (powerless/identity_locked) — bears constrained legal and marital autonomy
 *   - contextual_egalitarian_scholars & progressive_abrogation_scholars: Excluded reformist voices (organized/constrained) — offer textually-grounded alternatives denied binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.72).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.72).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Literal-Hierarchical Reading of Qur'anic Gender Verses (4:11, 2:282, 4:34)").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious_legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '03678276-91fd-4d05-a54c-9949a8bce072').
narrative_ontology:cs_kernel_codification('03678276-91fd-4d05-a54c-9949a8bce072', fixed_text).
narrative_ontology:cs_authority_grounding('03678276-91fd-4d05-a54c-9949a8bce072', lineage).
narrative_ontology:cs_interpretation_layer_present('03678276-91fd-4d05-a54c-9949a8bce072').
narrative_ontology:cs_reading_relation('03678276-91fd-4d05-a54c-9949a8bce072', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('03678276-91fd-4d05-a54c-9949a8bce072', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('03678276-91fd-4d05-a54c-9949a8bce072', foundational, verses_constitute_fixed_non_abrogable_legal_ordinance).
narrative_ontology:cs_axiom_status(verses_constitute_fixed_non_abrogable_legal_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('03678276-91fd-4d05-a54c-9949a8bce072', verses_constitute_fixed_non_abrogable_legal_ordinance, theological).
narrative_ontology:cs_axiom('03678276-91fd-4d05-a54c-9949a8bce072', foundational, male_guardianship_is_divinely_mandated_household_structure).
narrative_ontology:cs_axiom_status(male_guardianship_is_divinely_mandated_household_structure, holdable).
narrative_ontology:cs_axiom_grounding('03678276-91fd-4d05-a54c-9949a8bce072', male_guardianship_is_divinely_mandated_household_structure, theological).
narrative_ontology:cs_reference_frame('03678276-91fd-4d05-a54c-9949a8bce072', classical_literalist_qiwamah_framework).
narrative_ontology:cs_drift_state('03678276-91fd-4d05-a54c-9949a8bce072', contemporary_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('03678276-91fd-4d05-a54c-9949a8bce072', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_court_authorities).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_witnesses).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, wives_under_guardianship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold formal guardianship (qiwamah) over wives and dependents under this reading, control household finances, exercise disciplinary authority sanctioned by 4:34, and receive double the inheritance share of female co-heirs under 4:11. They administer family affairs and can invoke religious-court backing when guardianship is contested.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter).

% Codify and enforce the literal reading in family law, inheritance adjudication, and testimony procedure. Administer courts that weight female testimony at half the value of male testimony per 2:282, adjudicate inheritance splits per 4:11, and rule on guardianship disputes per 4:34. Their institutional legitimacy and jurisdiction depend on the verses being treated as fixed legal ordinance rather than historically contingent guidance.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_court_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Receive a codified inheritance share double that of female siblings under this reading of 4:11. Benefit passively from a rule they did not design but which structurally advantages them in estate division, with courts enforcing the split without requiring individual advocacy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_heirs, beneficiary,
    moderate, biographical, mobile, national).

% Receive half the inheritance share of male co-heirs under the literal application of 4:11. Contesting the split requires litigating in courts whose interpretive framework already treats the ratio as divinely fixed; exit means either accepting the reduced share, migrating to a jurisdiction with different family law, or facing social and religious sanction for challenging the ruling.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_heirs, payer,
    powerless, biographical, trapped, national).

% Under 2:282's literal application to financial contract witnessing (and extended by courts to broader legal contexts), a woman's testimony is weighted at half a man's, requiring a second female witness to corroborate. This constrains their standing in contract disputes, financial litigation, and in some jurisdictions criminal proceedings, regardless of individual competence or evidentiary reliability.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_witnesses, payer,
    powerless, immediate, trapped, national).

% Live under a husband's guardianship authority as codified from 4:34, including provisions read as permitting graduated disciplinary escalation for perceived disobedience (nushuz). Exit requires divorce processes that are frequently asymmetric (unilateral talaq favors the husband; khula for the wife often requires his consent or court intervention), and apostasy or open renunciation of the framework carries severe social, familial, and in some jurisdictions legal consequences.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, wives_under_guardianship, payer,
    powerless, biographical, identity_locked, local).

% Argue the same verses are 7th-century progressive interventions to be reinterpreted through overarching Qur'anic equity principles (maqasid). Their reformist jurisprudence is frequently excluded from state-sanctioned religious court curricula and delegitimized as unfaithful to the text; they operate mostly in academic and NGO spaces rather than binding adjudicative authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, contextual_egalitarian_scholars, excluded,
    organized, generational, constrained, national).

% Argue later universal-dignity verses (e.g., 49:13) supersede earlier gender-specific rules via naskh. Their position is marginal within mainstream classical jurisprudence, which generally restricts abrogation to specific textual and chronological criteria that this reading's proponents dispute apply here; they are rarely given standing in religious courts governed by the literal-hierarchical tradition.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, progressive_abrogation_scholars, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, textually-anchored rule set for inheritance division, contract witnessing, and household authority that removes case-by-case negotiation and gives religious courts a stable, citable basis for adjudication across a large population.
% TRANSFER_FUNCTION: Moves inheritance share, legal testimonial weight, and household decision-making authority from women to men; moves interpretive authority from reformist scholarship to religious court institutions that administer the literal reading.
% ABSENT_VOICES: Contextual-egalitarian and progressive-abrogation scholars are structurally excluded from binding adjudication despite offering textually-grounded alternative readings; women subject to the rulings are rarely represented in the bodies that interpret the verses governing them.
% DISAPPEARANCE_RATIONALE: If the literal-hierarchical reading's institutional enforcement vanished, inheritance division, testimony weighting, and guardianship authority in the jurisdictions that codify it would immediately require alternative legal bases; male heirs and household heads would lose a default structural advantage currently backed by state-sanctioned religious courts, and women would gain legal standing currently withheld under this reading.
% FOUNDING_PROBLEM: 7th-century Arabia lacked codified inheritance protection for women (who previously often received nothing), lacked standardized commercial contract evidentiary procedure, and lacked defined household support and protection obligations in a tribal society with high female vulnerability to abandonment and economic precarity.
% FOUNDING_PROBLEM_CORROBORATION: Religious court authorities and male household heads attest the founding problem's solution remains divinely fixed and permanently binding. Contextual-egalitarian scholars and progressive-abrogation scholars, both external to the beneficiary set, attest the founding problem (protecting women in a context of zero prior inheritance rights) has been substantially resolved or transformed by modern economic and legal conditions, and that the literal ratio no longer serves the protective function it was originally read to serve — comparative legal historians outside all three religious factions corroborate that women's economic and legal position has changed structurally since the 7th century in ways the literal reading does not adjust for.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the concrete, quantifiable transfer of inheritance share, testimonial weight, and decision-making authority from women to men under this reading's codified application. Suppression (0.78) is high and rising over the measured interval because maintaining the literal reading against a growing body of reformist scholarship and shifting social conditions requires increasingly active institutional enforcement (family court rulings, apostasy and family-rupture costs for exit, social sanction for public dissent) rather than passive acceptance. Theater ratio is low (0.20) — the guardianship and inheritance functions this reading enforces are substantively, not performatively, operative in the jurisdictions that codify it; there is little disconnect between stated function and actual practice. Accessibility collapse (0.62) is moderate-high: alternatives (contextual and abrogation readings) exist and are actively argued by scholars, but are structurally locked out of binding legal authority, so from inside the affected population's practical legal reality, alternatives have substantially but not completely collapsed. Resistance (0.58) reflects active, organized reformist and feminist jurisprudential pushback, which is real but currently non-binding.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and religious court authorities sit near the full-beneficiary end: they collect guardianship authority, resource control, and interpretive legitimacy directly from the arrangement, with mobile-to-arbitrage exit options (they can invoke the framework selectively or shift jurisdictions while retaining status). Male heirs benefit passively and structurally without needing to defend the arrangement. Women heirs, female witnesses, and wives under guardianship sit near the full-target end: constrained or trapped exit, direct extraction of inheritance share, testimonial standing, or marital autonomy, and high identity-lock costs (apostasy, family rupture, social sanction) for attempting exit. This asymmetry is the coordination/extraction split the tangled_rope classification requires: courts genuinely coordinate a stable, citable inheritance and evidentiary procedure across a large population (coordination function), while that same procedure asymmetrically transfers value and standing from women to men (extraction function) — both are present in the same structure, which is why tangled_rope rather than pure rope or pure snare is the structurally accurate claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing (protecting women in a 7th-century context of zero prior inheritance rights) is contested precisely because it may have been substantially resolved by modern legal and economic conditions while the arrangement persists via institutional inertia and religious-court authority interests. The mismatch between founding_problem_status='contested' and disappearance_verdict='world_rearranges' is exactly the signal this framework is built to surface: reformist scholars argue the original protective function is largely dead while the extractive structural consequence (halved inheritance, halved testimony, guardianship authority) remains fully live and enforced — that gap between dead function and live extraction is a candidate zombie-mandate pattern, distinct from the sibling readings' own accounts of whether the mandate ever required this literal form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_gender_verses,
    'Is the literal-hierarchical reading of 4:11/2:282/4:34 the theologically correct and historically intended reading, or is it one of at least three defensible readings (literal_hierarchical, contextual_egalitarian, progressive_abrogation) contesting the same textual kernel?',
    'No empirical resolution mechanism exists within the framework — this is a live doctrinal and interpretive dispute among qualified religious authorities and scholars, resolved (if at all) by shifts in the interpretive authority structure of religious courts and Islamic jurisprudential consensus (ijma) over time, not by external adjudication.',
    'If the contextual_egalitarian or progressive_abrogation readings gain binding institutional authority, the extraction this story documents would be reclassified from divine ordinance to historically contingent and revisable legal practice, collapsing much of the accessibility_collapse metric and reducing suppression requirements sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_gender_verses, conceptual, 'Which of three sibling readings of the same kernel verses holds binding interpretive authority.').

omega_variable(
    natural_ordinance_vs_constructed_hierarchy,
    'Is the guardianship/inheritance/testimony structure this reading enforces a fixed, unchangeable feature of divine law (as the literal reading claims), or a constructed legal hierarchy that benefits identifiable agents (male household heads, religious court authorities) and could be otherwise?',
    'Comparative jurisprudential analysis across Muslim-majority jurisdictions that have reformed family law (e.g., Tunisia''s Code of Personal Status) versus those retaining the literal codification, tracking whether reform correlates with measurable shifts in the beneficiary structure without triggering the theological collapse the literal reading''s proponents predict.',
    'If reform jurisdictions show the coordination function (stable inheritance/evidentiary procedure) persists without the extractive asymmetry, this supports treating the literal reading''s hierarchy as constructed rather than an irreducible feature of the faith; if reform consistently produces the doctrinal instability literalist scholars predict, this weakens the constructed-hierarchy account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_ordinance_vs_constructed_hierarchy, empirical, 'Whether the enforced hierarchy is a natural feature of the faith or a constructed, benefit-concentrating legal structure.').

omega_variable(
    exit_cost_measurement_ambiguity,
    'How much of the measured ''trapped''/''identity_locked'' exit-option classification for women under this reading reflects genuinely irreducible religious/family costs versus state-enforced legal penalties (apostasy law, unilateral divorce asymmetry) that could be legislatively altered independent of the theological reading itself?',
    'Track exit-cost variance across jurisdictions applying the same literal reading but differing in state civil-law backing (e.g., whether apostasy carries criminal penalty vs. purely social consequence).',
    'If exit costs vary substantially with state law while the theological reading remains constant, this indicates part of the measured suppression is a separable, and more contestable, state-enforcement layer rather than an inherent feature of the literal reading — this would refine but not eliminate the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement_ambiguity, empirical, 'Separating theologically-inherent suppression from state-enforcement-layer suppression in exit cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__literal_hierarchical, theater_ratio, 8, 0.14).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__literal_hierarchical, theater_ratio, 16, 0.16).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__literal_hierarchical, theater_ratio, 24, 0.18).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__literal_hierarchical, theater_ratio, 32, 0.19).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__literal_hierarchical, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__literal_hierarchical, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__literal_hierarchical, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__literal_hierarchical, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__literal_hierarchical, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__literal_hierarchical, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__literal_hierarchical, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__literal_hierarchical, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__literal_hierarchical, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__literal_hierarchical, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__literal_hierarchical, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, enforcement_mechanism).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the same kernel text (verses 4:11, 2:282, 4:34). literal_hierarchical (this file) claims tangled_rope with high extraction concentrated on women heirs, female witnesses, and wives under guardianship. contextual_egalitarian and progressive_abrogation are separate files with their own ε, beneficiary/victim sets, and likely lower extraction profiles given their reformist coordination emphasis. All three are linked bidirectionally via affects_constraints since institutional gains by any one reading structurally affect the legitimacy conditions and resource availability of the others (e.g., state adoption of literal_hierarchical family law directly forecloses court-level standing for the sibling readings' proponents).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
