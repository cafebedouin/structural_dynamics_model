% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Literal-Hierarchical Reading of Quranic Gender Verses (4:11, 2:282, 4:34)
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This constraint story models the literal-hierarchical reading of Quranic
 *   verses 4:11 (inheritance), 2:282 (testimony), and 4:34 (guardianship) as
 *   a standing legal arrangement. The reading treats these verses as
 *   timeless, context-independent divine ordinances establishing male
 *   authority over women in family, property, and legal procedure. The
 *   constraint operates through religious courts, family law codes, and
 *   social enforcement in Muslim-majority jurisdictions and diaspora
 *   communities. The reading claims mountain status (divine law,
 *   unchangeable), but the authored metrics describe a high-extraction,
 *   actively enforced arrangement with identity-locked victims — the engine
 *   will compute per-seat classifications from this structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Literal-Hierarchical Reading of Quranic Gender Verses (4:11, 2:282, 4:34)").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '45839233-8596-4ab1-8fba-33ed58d1f143').
narrative_ontology:cs_kernel_codification('45839233-8596-4ab1-8fba-33ed58d1f143', fixed_text).
narrative_ontology:cs_authority_grounding('45839233-8596-4ab1-8fba-33ed58d1f143', lineage).
narrative_ontology:cs_interpretation_layer_present('45839233-8596-4ab1-8fba-33ed58d1f143').
narrative_ontology:cs_reading_relation('45839233-8596-4ab1-8fba-33ed58d1f143', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('45839233-8596-4ab1-8fba-33ed58d1f143', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('45839233-8596-4ab1-8fba-33ed58d1f143', foundational, verses_are_timeless_divine_law).
narrative_ontology:cs_axiom_status(verses_are_timeless_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('45839233-8596-4ab1-8fba-33ed58d1f143', verses_are_timeless_divine_law, deontological).
narrative_ontology:cs_axiom('45839233-8596-4ab1-8fba-33ed58d1f143', foundational, male_guardianship_is_divinely_ordained).
narrative_ontology:cs_axiom_status(male_guardianship_is_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('45839233-8596-4ab1-8fba-33ed58d1f143', male_guardianship_is_divinely_ordained, deontological).
narrative_ontology:cs_axiom('45839233-8596-4ab1-8fba-33ed58d1f143', secondary, gendered_inheritance_shares_are_fixed_math).
narrative_ontology:cs_axiom_status(gendered_inheritance_shares_are_fixed_math, holdable).
narrative_ontology:cs_axiom_grounding('45839233-8596-4ab1-8fba-33ed58d1f143', gendered_inheritance_shares_are_fixed_math, deontological).
narrative_ontology:cs_reference_frame('45839233-8596-4ab1-8fba-33ed58d1f143', classical_fiqh_framework).
narrative_ontology:cs_drift_state('45839233-8596-4ab1-8fba-33ed58d1f143', contemporary_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('45839233-8596-4ab1-8fba-33ed58d1f143', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, traditionalist_scholarly_establishment).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_litigation).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, male_guardianship_doctrine).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_ordinance_claim).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, gendered_inheritance_fixed_shares).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, male_testimony_weight_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal authority over female dependents (wives, daughters, sisters) in marriage, divorce, travel, financial decisions, and guardianship. Receive double inheritance shares under 4:11. Their testimony counts equally with other men in court. Exit from the constraint is easy — they simply exercise the rights the reading grants them.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    moderate, biographical, mobile, local).

% Adjudicate family law, inheritance, and personal status cases under the literal-hierarchical reading. Their institutional authority and relevance depend on being the authorized interpreters and enforcers of these verses. They collect fees, control procedural outcomes, and gatekeep access to religious legitimacy. Exit would mean ceding jurisdiction to secular courts — a loss of institutional power.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce fatwas, commentaries, and curricula that legitimize the literal-hierarchical reading. Their scholarly capital, institutional positions, and donor networks are built on defending this reading as the only authentic Islam. Exit would require repudiating their life's work and institutional base — professionally and socially costly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, traditionalist_scholarly_establishment, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, traditionalist_scholarly_establishment, agenda_setter).

% Subject to male guardian permission for marriage, travel, medical decisions, and legal agency. The constraint structures their entire civic personality. Exit requires either guardian consent (which the constraint makes unlikely), secular legal intervention (often unavailable or dangerous), or apostasy/family rupture — each carrying severe social, economic, and sometimes physical risk. Their Islamic identity is fused with the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_under_guardianship, payer,
    powerless, biographical, identity_locked, local).

% Receive half the inheritance share of male counterparts under 4:11. The loss is direct, quantifiable, and non-negotiable within the reading's framework. No exit exists within the system — the verse is treated as fixed divine math. Challenging it risks accusations of opposing God's ordinance.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    powerless, immediate, trapped, local).

% Under 2:282, their testimony counts as half a man's in financial contracts, and in many schools extends to all legal matters. This structurally weakens their capacity to prove claims, defend property, or secure justice. The constraint operates at the moment of legal need — exit is impossible because the rule applies precisely when they need the law's protection.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_litigation, payer,
    powerless, immediate, trapped, local).

% Advocate contextual-egalitarian or progressive-abrogation readings using maqasid, naskh, or historical-critical methods. They are marginalized from mainstream scholarly institutions, denied platforms in state religious apparatuses, and often face apostasy accusations. Their exclusion is structural — the literal-hierarchical reading's authority depends on silencing them.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reformist_scholars, excluded,
    moderate, biographical, constrained, global).

% Constitutional courts and legislatures in Muslim-majority and minority contexts that must decide whether to recognize, accommodate, or override the literal-hierarchical reading in personal status law. They observe the constraint's operation from outside its epistemic framework but their rulings directly affect its enforcement reach.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, unambiguous framework for family formation, inheritance distribution, and legal testimony that resolves disputes by reference to a text treated as beyond human revision — eliminating negotiation costs in patriarchal household economies.
% TRANSFER_FUNCTION: Moves legal authority, inheritance wealth, testimonial weight, and guardianship rights from women to male household heads and religious courts. The transfer is justified as divine distribution, not human arrangement.
% ABSENT_VOICES: Women living under the constraint who would reject it if exit were safe — particularly those experiencing guardianship abuse, inheritance dispossession, or testimonial injustice. They are structurally silenced by the identity_locked exit condition: speaking against the reading is framed as speaking against Islam itself. Also absent: pre-modern women's voices from the formative period, whose absence is read as consensus.
% DISAPPEARANCE_RATIONALE: If the literal-hierarchical reading vanished overnight, inheritance would need new distribution rules, guardianship laws would collapse, testimony rules would require replacement, and religious courts would lose their core family-law jurisdiction. The entire personal-status legal architecture in multiple countries would require reconstruction. The world rearranges because the constraint is the load-bearing wall of that architecture.
% FOUNDING_PROBLEM: 7th-century Arabian tribal society needed a stable, textually anchored system to replace fluid customary practices governing inheritance, marriage, and testimony — a system that could unify a rapidly expanding polity under a single legal vocabulary claimed as divine.
% FOUNDING_PROBLEM_CORROBORATION: Classical fiqh historians (e.g., Wael Hallaq, Kecia Ali) document that the founding problem — unifying tribal customary law under a revelation-based framework — was specific to the formative period. Contemporary traditionalist scholars attest the problem is live (divine law is timeless), but this attestation comes from within the beneficiary set. No corroborating source outside the traditionalist establishment affirms that 7th-century unification needs justify 21st-century gender-differentiated rights.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint transfers concrete resources (inheritance shares), legal capacity (testimony weight, guardianship), and civic autonomy from women to men and institutions, with no reciprocal flow. Suppression (0.78) is high because the constraint's persistence depends on criminalizing apostasy, stigmatizing dissent, and denying women exit options — not on participant consent. Theater ratio (0.45) is moderate: the coordination function (dispute resolution via fixed rules) is real but increasingly performative as the rules diverge from lived reality; enforcement energy goes toward maintaining the gender hierarchy, not solving coordination problems. Accessibility collapse (0.88) is near-maximal because the reading frames alternatives as heresy — once the constraint is understood as divine ordinance, alternatives are cognitively and socially collapsed. Resistance (0.55) is moderate: reform movements exist but face severe repression; resistance is ongoing but structurally contained.
 *
 * PERSPECTIVAL GAP:
 *   From the male_household_head and religious_court seats, the constraint appears as genuine coordination (rope-like): it provides clear rules, reduces conflict, and reflects divine wisdom. From the women_under_guardianship, female_heirs, and women_in_litigation seats, the same structure operates as pure extraction (snare) — they pay the costs, cannot exit, and the coordination story is cover. The engine computes this divergence from the structural data; the claimed_type (snare) reflects the authoring seat's judgment that the extraction is primary and the coordination is secondary/cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads are structural beneficiaries (d near 0.0) — they collect inheritance surplus, guardianship authority, and testimonial advantage with mobile exit. Religious courts and traditionalist scholars are agenda_setters who also benefit (d near 0.1-0.2) — they administer the constraint and derive institutional legitimacy from it. Women in all three victim roles are identity_locked or trapped (d near 0.9-1.0) — they bear the full extractive weight with exit costs that include loss of family, community, safety, and religious identity. The identity_lock for women_under_guardianship is relational: their self-concept and communal belonging are constituted through the very relationship the constraint governs. Reformist scholars are excluded (d not computed — they are outside the constraint's operation). Secular legal systems are observers (d=0.5 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (7th-century legal unification) is dead, but the arrangement persists and has intensified (extraction rising from 0.75 to 0.82 over 1400 years). This is mandatrophy: the constraint's mandate has outlived its function, but the beneficiaries (male heads, courts, scholars) capture enough value to maintain it, while the victims are too identity-locked to dismantle it. The theater ratio rise (0.25 to 0.45) shows increasing performative maintenance — more energy spent defending the reading's authenticity than solving coordination problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the literal-hierarchical reading a genuine recovery of authorial intent, or a constructed interpretation that serves patriarchal interests?',
    'Comparative historical analysis of early tafsir diversity, pre-classical legal practice, and the emergence of the ''literalist'' hermeneutic as a response to colonial modernity.',
    'If constructed, the constraint''s claimed mountain status (divine ordinance) is a false summit — the beneficiary structure (male heads, courts, scholars) would be revealed as the reading''s actual foundation, triggering FSM reclassification to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the reading''s mountain claim is genuine or constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, family coercion) or internalized (women''s belief in the reading''s divine legitimacy)?',
    'Post-exit suppression trajectory studies: if women who exit the constraint (via secular law, migration, or apostasy) continue to self-censor or feel religious guilt, internalized suppression is significant.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists in the subject after formal exit. This would increase the constraint''s extraction efficiency without additional enforcement cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-locked interpersonal constraints.').

omega_variable(
    coordination_extraction_boundary,
    'Does the literal-hierarchical reading solve a genuine coordination problem (stable inheritance, clear testimony rules) that would otherwise require costly negotiation, or is the coordination story entirely post-hoc cover?',
    'Counterfactual modeling: simulate dispute rates and transaction costs in family/property matters under alternative rule-sets (egalitarian, customary, secular) in comparable populations.',
    'If genuine coordination exists, the constraint is tangled_rope (coordination + extraction). If coordination is negligible or worse than alternatives, it is pure snare. The claimed_type (snare) assumes the latter; this omega tracks the uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint has a non-extractive coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_lh_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qgv_lh_tr_t350, quranic_gender_verses__literal_hierarchical, theater_ratio, 350, 0.3).
narrative_ontology:measurement(qgv_lh_tr_t700, quranic_gender_verses__literal_hierarchical, theater_ratio, 700, 0.38).
narrative_ontology:measurement(qgv_lh_tr_t1050, quranic_gender_verses__literal_hierarchical, theater_ratio, 1050, 0.42).
narrative_ontology:measurement(qgv_lh_tr_t1400, quranic_gender_verses__literal_hierarchical, theater_ratio, 1400, 0.45).

% Extraction over time
narrative_ontology:measurement(qgv_lh_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(qgv_lh_be_t350, quranic_gender_verses__literal_hierarchical, base_extractiveness, 350, 0.78).
narrative_ontology:measurement(qgv_lh_be_t700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 700, 0.8).
narrative_ontology:measurement(qgv_lh_be_t1050, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1050, 0.81).
narrative_ontology:measurement(qgv_lh_be_t1400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1400, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qgv_lh_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qgv_lh_su_t350, quranic_gender_verses__literal_hierarchical, suppression_requirement, 350, 0.7).
narrative_ontology:measurement(qgv_lh_su_t700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 700, 0.73).
narrative_ontology:measurement(qgv_lh_su_t1050, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1050, 0.76).
narrative_ontology:measurement(qgv_lh_su_t1400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1400, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.08).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, fiqh_personal_status_codes).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, apostasy_laws).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, male_guardianship_regulations).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_inheritance_courts).

% DUAL FORMULATION NOTE:
% This constraint is the literal_hierarchical reading of the quranic_gender_verses kernel. It differs structurally from sibling readings: contextual_egalitarian has lower extractiveness (reinterpretation reduces gender differentiation) and different victim sets; progressive_abrogation has contested extractiveness depending on which principles are deemed abrogating. All three share the same textual referent but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, institutional, 0.15).
constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
