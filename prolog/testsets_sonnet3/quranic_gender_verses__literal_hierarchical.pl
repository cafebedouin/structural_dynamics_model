% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Literal-Hierarchical Reading of Quranic Gender Verses (4:11, 2:282, 4:34) as Fixed Divine Law
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   In jurisdictions and communities where these three verses are read as
 *   fixed, timeless divine ordinance, the reading is institutionalized into
 *   personal-status law, inheritance courts, and evidentiary procedure. Male
 *   household heads gain guardianship authority and a doubled inheritance
 *   share; women subject to the arrangement face constrained legal voice,
 *   reduced inheritance, and discounted courtroom testimony. The coordination
 *   function — settled, citable rules that reduce case-by-case dispute — is
 *   real, but it rides alongside asymmetric extraction sustained by clerical
 *   and judicial enforcement, which is why this reading is authored as
 *   tangled_rope rather than mountain or pure snare: there is a genuine
 *   coordination good (legal predictability, an original historical
 *   improvement over total exclusion) bundled with ongoing extraction from a
 *   clearly identifiable victim class.
 *
 * KEY AGENTS:
 *   - male_household_heads: beneficiary/agenda_setter (powerful/arbitrage) — collects guardianship authority and doubled inheritance
 *   - religious_court_authorities: agenda_setter (institutional/analytical) — administers and enforces the literal reading
 *   - wives_under_guardianship: payer (powerless/trapped) — bears constrained autonomy and financial dependency
 *   - female_heirs: payer (powerless/trapped) — bears halved inheritance share
 *   - female_witnesses_in_court: payer (powerless/constrained) — bears discounted legal testimony weight
 *   - contextual_egalitarian_scholars and progressive_abrogation_scholars: excluded (organized/constrained) — sibling readings kept out of institutional interpretive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.78).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.72).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Literal-Hierarchical Reading of Quranic Gender Verses (4:11, 2:282, 4:34) as Fixed Divine Law").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '1d2dbc5b-662a-4c87-a934-217589df283e').
narrative_ontology:cs_kernel_codification('1d2dbc5b-662a-4c87-a934-217589df283e', fixed_text).
narrative_ontology:cs_authority_grounding('1d2dbc5b-662a-4c87-a934-217589df283e', lineage).
narrative_ontology:cs_interpretation_layer_present('1d2dbc5b-662a-4c87-a934-217589df283e').
narrative_ontology:cs_reading_relation('1d2dbc5b-662a-4c87-a934-217589df283e', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('1d2dbc5b-662a-4c87-a934-217589df283e', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('1d2dbc5b-662a-4c87-a934-217589df283e', foundational, textual_literalism_yields_timeless_law).
narrative_ontology:cs_axiom_status(textual_literalism_yields_timeless_law, holdable).
narrative_ontology:cs_axiom_grounding('1d2dbc5b-662a-4c87-a934-217589df283e', textual_literalism_yields_timeless_law, theological).
narrative_ontology:cs_axiom('1d2dbc5b-662a-4c87-a934-217589df283e', foundational, gender_differentiated_rules_are_permanent_divine_ordinance).
narrative_ontology:cs_axiom_status(gender_differentiated_rules_are_permanent_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('1d2dbc5b-662a-4c87-a934-217589df283e', gender_differentiated_rules_are_permanent_divine_ordinance, theological).
narrative_ontology:cs_reference_frame('1d2dbc5b-662a-4c87-a934-217589df283e', classical_literalist_jurisprudence).
narrative_ontology:cs_drift_state('1d2dbc5b-662a-4c87-a934-217589df283e', contemporary_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1d2dbc5b-662a-4c87-a934-217589df283e', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_court_authorities).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, conservative_clerical_establishment).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, wives_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_witnesses_in_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold qiwama (guardianship) authority over wives under this reading of 4:34, control household finances and decision-making, and receive a double inheritance share relative to female co-heirs under 4:11. Can exit unfavorable family arrangements (divorce initiation, remarriage) far more freely than the women bound to them.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter).

% Administer family law, inheritance division, and contract witnessing under this literal reading. Apply the two-women-to-one-man testimony ratio from 2:282 and adjudicate guardianship disputes citing 4:34. Their institutional authority and caseload depend on the verses being read as fixed, unrevisable law rather than historically contingent guidance.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_court_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Derive doctrinal authority, teaching positions, and social standing from being the recognized interpreters of literal, timeless divine ordinance. A shift toward contextual or abrogationist readings would erode their exclusive claim to correct interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, conservative_clerical_establishment, beneficiary,
    institutional, civilizational, arbitrage, global).

% Subject to a husband's qiwama authority over financial and disciplinary matters under 4:34. Exiting the marriage risks loss of custody, community standing, and religious legitimacy; in jurisdictions that codify this reading into civil law, exit may also mean loss of legal residency or state benefits tied to marital status.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, wives_under_guardianship, payer,
    powerless, biographical, trapped, national).

% Receive half the inheritance share of an equivalently positioned brother under the literal reading of 4:11. Contesting the division requires litigation in courts staffed by authorities who treat the ratio as fixed law, and social pressure discourages formal challenge within the family.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    powerless, generational, trapped, national).

% Under the literal reading of 2:282, a woman's testimony in commercial/contractual matters is weighted at half a man's and requires corroboration by a second woman. This structurally discounts their legal voice in disputes, including disputes over their own inheritance or marital claims.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_witnesses_in_court, payer,
    powerless, biographical, constrained, national).

% Argue the verses were progressive correctives to pre-Islamic Arabian norms and should be reread through maqasid (higher objectives) of equity. Marginalized from official fatwa-issuing bodies and religious court appointments in jurisdictions where the literal reading holds institutional authority; their reading is a separate constraint, not adjudicated here.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, contextual_egalitarian_scholars, excluded,
    organized, generational, constrained, global).

% Argue later universalist verses (e.g. 49:13) supersede the gender-differentiated rules via naskh. Excluded from mainstream jurisprudential authority in most literalist-dominant legal systems; treated by the literal-hierarchical establishment as doctrinally illegitimate rather than as a live juristic option.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, progressive_abrogation_scholars, excluded,
    organized, generational, constrained, global).

% Study how this reading is codified into personal-status and inheritance law across jurisdictions, and compare outcomes against sibling readings and against secular legal frameworks. Take no side in enforcement but document differential effects on women's economic and legal standing.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, widely recognized rule-set for inheritance division, courtroom evidentiary weight, and household authority that removes case-by-case negotiation and gives religious courts a settled, citable basis for rulings.
% TRANSFER_FUNCTION: Moves inheritance share, legal testimonial weight, and household decision-making authority from women to their male relatives and male-headed courts, formalized as fixed religious obligation rather than negotiable custom.
% ABSENT_VOICES: Contextual-egalitarian and progressive-abrogation scholars would contest the timelessness claim itself, arguing the verses are historically situated or superseded; they are structurally excluded from religious court appointments and official fatwa authority in jurisdictions where this reading is institutionalized. Women directly subject to the rulings are rarely represented in the interpretive bodies that apply them.
% DISAPPEARANCE_RATIONALE: If this literal-hierarchical reading were displaced by a sibling reading (or by secular codification), inheritance shares would equalize or shift, courtroom testimony weighting would change, and household guardianship authority would no longer carry the force of unrevisable divine law — family law systems and clerical institutional structures built on this reading would need to reorganize their legal and doctrinal basis.
% FOUNDING_PROBLEM: Originally addressed 7th-century Arabian conditions: protecting women's property rights where none previously existed (partial inheritance was itself an advance over total exclusion), establishing enforceable maintenance obligations on male relatives, and creating verifiable commercial record-keeping in a largely oral, low-literacy economy (the two-witness rule for financial contracts in 2:282 names women's typically lesser involvement in commercial transactions of that era, not an assessment of capacity).
% FOUNDING_PROBLEM_CORROBORATION: The literal-hierarchical establishment attests the ordinance remains fully live and unconditional divine command. Contextual-egalitarian and progressive-abrogation scholars — outside this reading's own beneficiary structure — attest the originating conditions (women's total pre-Islamic exclusion from inheritance, near-universal female commercial non-participation, absence of independent economic infrastructure for women) have substantially changed in many contemporary Muslim-majority societies, and that literalist courts largely do not adjudicate this status question themselves.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78) because the literal reading concentrates decision-making, financial control, and courtroom credibility asymmetrically by gender, and this asymmetry is actively defended rather than incidental. Suppression (0.72) reflects that exit from the arrangement (apostasy, divorce contest, testimony challenge) carries severe social, legal, and in some jurisdictions civil penalties — this is a raw structural property, not scaled by scope. Theater ratio is kept low (0.2) because the enforcement is substantively functional, not primarily performative: courts actually apply differentiated inheritance and testimony rules, they do not merely gesture at them. Accessibility collapse (0.6) is moderate-high because once codified into personal-status law, alternative readings become practically inaccessible to individuals inside the system, though the sibling readings remain live in scholarly and activist discourse. Resistance (0.68) reflects substantial organized contestation from reformist and feminist Islamic scholarship, distinguishing this from a genuine, uncontested mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of male household heads and religious court authorities, the arrangement reads as settled divine coordination requiring no further justification. From the seat of wives, female heirs, and female witnesses, the identical structure operates as enforced asymmetric extraction with high exit cost. The engine computes this divergence from the declared power/exit/beneficiary structure; the claimed_type (tangled_rope) is authored independently of which seat's perception is privileged.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and religious court authorities sit near the full-beneficiary end: they collect the coordination benefit (settled rules) and structural authority (guardianship, adjudication power) with high exit options of their own. Wives, female heirs, and female witnesses sit near the full-target end: the same rules extract inheritance share, financial autonomy, and legal voice from them, and their exit options are trapped or heavily constrained by apostasy risk, custody loss, and social rupture. The clerical establishment benefits indirectly through doctrinal authority rather than direct household control, placing it closer to the beneficiary end but via a different mechanism than the household heads.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting women's property rights and household maintenance obligations in a context of total prior exclusion) was, by the corroborating record outside this reading's own beneficiary set, substantially addressed by later legal and economic developments in many contemporary contexts. Classifying this as tangled_rope rather than snare acknowledges the genuine historical coordination function it originally solved, while classifying it as tangled_rope rather than mountain or rope acknowledges that, under the literal-hierarchical reading's own insistence on timelessness, the arrangement continues extracting from an identifiable victim class long after the specific originating conditions the corroborating sources describe have changed — this is exactly the mandatrophy pattern: a coordination mandate persisting past its function, defended as unrevisable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_constructed_naturality,
    'Is the timeless, unrevisable status this reading claims for verses 4:11, 2:282, and 4:34 a property of the text itself, or a constructed interpretive choice that benefits identifiable agents (male household heads, religious court authorities, the clerical establishment)?',
    'Comparative textual and historical analysis of pre-Islamic Arabian legal norms against the verses'' stipulations (to assess whether the rules were reformist relative to their context), combined with comparison across the three sibling readings'' treatment of the same verses and their differing conclusions about revisability.',
    'If the arrangement is better understood as a historically contingent reform rather than a timeless ordinance, the literal_hierarchical reading''s claim to be describing fixed natural/divine law is undermined, supporting reclassification toward the sibling readings'' egalitarian or abrogationist conclusions rather than a divine mandate immune to revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_vs_constructed_naturality, conceptual, 'Whether this reading''s timelessness claim is textually intrinsic or an interpretive construction serving identifiable beneficiaries.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly does this reading diverge from its siblings — in the interpretation of the Arabic legal terminology (e.g., qiwama, daraja), in the theory of abrogation (naskh) applied to later verses, or in the methodological weight given to maqasid (higher objectives) versus literal textual force?',
    'Systematic classical and contemporary tafsir (exegesis) comparison isolating the specific hermeneutic move (literal semantic reading vs. maqasid-based contextualization vs. naskh doctrine) that produces each reading''s divergent legal conclusion.',
    'Locating the exact point of divergence clarifies whether the three readings are separable at the level of individual verses (allowing partial convergence, e.g., accepting contextualization for 2:282 while disputing it for 4:34) or are only separable as complete interpretive packages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Structural location of the interpretive disagreement between literal_hierarchical and its sibling readings.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination benefit (settled inheritance and evidentiary rules, reduced litigation) separable from the extractive asymmetry (unequal inheritance share, discounted testimony, male guardianship authority), or does the literal reading''s coordination function depend structurally on maintaining that asymmetry?',
    'Examine jurisdictions or historical periods where partial reform occurred (e.g., equal-inheritance civil codes coexisting with religious courts) to test whether legal predictability and dispute-reduction persisted without the gendered asymmetry.',
    'If separable, the arrangement is better modeled as extraction riding on a genuinely separable coordination function (supporting reform without abandoning legal predictability); if inseparable, the tangled_rope classification understates how tightly extraction is fused to the coordination good under this specific reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the literal reading''s coordination function can be preserved while removing its extractive asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__literal_hierarchical, theater_ratio, 20, 0.12).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__literal_hierarchical, theater_ratio, 40, 0.14).
narrative_ontology:measurement(qura_tr_t60, quranic_gender_verses__literal_hierarchical, theater_ratio, 60, 0.16).
narrative_ontology:measurement(qura_tr_t80, quranic_gender_verses__literal_hierarchical, theater_ratio, 80, 0.18).
narrative_ontology:measurement(qura_tr_t100, quranic_gender_verses__literal_hierarchical, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__literal_hierarchical, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__literal_hierarchical, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(qura_be_t60, quranic_gender_verses__literal_hierarchical, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(qura_be_t80, quranic_gender_verses__literal_hierarchical, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(qura_be_t100, quranic_gender_verses__literal_hierarchical, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__literal_hierarchical, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__literal_hierarchical, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(qura_su_t60, quranic_gender_verses__literal_hierarchical, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(qura_su_t80, quranic_gender_verses__literal_hierarchical, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(qura_su_t100, quranic_gender_verses__literal_hierarchical, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.1).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quranic_gender_verses kernel. quranic_gender_verses__contextual_egalitarian treats the same verses as historically situated reformist steps requiring reinterpretation under maqasid principles (expected lower authored extractiveness, since the reading endorses ongoing reinterpretation rather than fixed hierarchy). quranic_gender_verses__progressive_abrogation treats later universalist verses as superseding the gender-specific rules via naskh (expected moderate authored extractiveness, since the doctrine acknowledges the earlier rules' historical validity while denying their present binding force). All three share the same underlying text and victim/beneficiary candidate pool but diverge sharply in claimed_type, extractiveness, and suppression because each reading authorizes a different degree of present-day enforceability. This file (literal_hierarchical) carries the highest authored extractiveness and suppression of the three because it is the only reading that treats the differentiated rules as currently, fully, and permanently binding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
