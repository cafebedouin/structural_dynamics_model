% ============================================================================
% CONSTRAINT STORY: equal_rights_amendment__sex_blind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_rights_amendment__sex_blind_reading, []).

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
 *   constraint_id: equal_rights_amendment__sex_blind_reading
 *   human_readable: ERA Sex-Blind Reading: Categorical Sex Classification Prohibition
 *   domain: constitutional_law/doctrinal_interpretation
 *
 * SUMMARY:
 *   The sex-blind reading of the Equal Rights Amendment commands that law may
 *   not classify individuals by sex under any circumstances, making sex
 *   analogous to race under strict scrutiny — the most stringent level of
 *   constitutional review. Under this reading, statutes providing
 *   sex-specific protections (pregnancy accommodation, nursing mother
 *   provisions, domestic violence shelters, sexual harassment law built on
 *   sex-based harm recognition) become constitutionally indefensible. The
 *   constraint instantiates one pole of a fundamental doctrinal contest over
 *   whether the ERA's mandate is formal equality (categorical sex-blindness)
 *   or anti-subordination (remediation of sex-based hierarchy). This story
 *   generates ONLY the sex-blind reading as a clean, ε-invariant constraint
 *   with its own beneficiaries (formal equality doctrine actors), victims
 *   (sex-conscious protective law), and extractiveness profile (0.68 — high
 *   extraction from remedial mechanisms, moderate doctrinal constraint). The
 *   competing anti-subordination reading is a separate constraint story with
 *   its own ε value, perspectives, and classification. The two readings
 *   coexist in contemporary constitutional discourse but foreclose each other
 *   within any single legal framework: a court cannot simultaneously hold
 *   that sex classification is categorically forbidden and that sex
 *   classification is permitted when it remedies subordination.
 *
 * KEY AGENTS:
 *   - Formal Equality Doctrine Claimants: Institutional beneficiaries (judges, legal scholars committed to formal equality, civil rights organizations favoring gender-neutral law) — capture the interpretive authority to constrain sex-based reasoning and establish categorical rules
 *   - Sex-Conscious Protective Law: Powerless victim — once the sex-blind reading is adopted, statutes protecting pregnant workers, nursing mothers, domestic violence survivors become per se unconstitutional; no exit mechanism for these laws
 *   - Beneficiaries of Sex-Conscious Remedial Law: Moderate victims (pregnant workers, nursing mothers, domestic violence survivors, sexual harassment plaintiffs) — face removal of statutory protections with constrained options to reorganize their protection
 *   - Anti-Subordination-Oriented Jurisprudence: Organized actor (law scholars, advocates committed to dismantling women's subordination) — constrained by the sex-blind reading's foreclosure of their preferred interpretive path, but retain capacity to develop state-level alternatives
 *   - State-Level Constitutional Systems: Organized actors with mobile exit options — can adopt anti-subordination readings of state ERAs, preserving sex-conscious remedial law at state level
 *   - Analytical Observer: Civilizational context — risks naturalizing the sex-blind reading as a formal-equality law of nature when it is a contested doctrinal choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_rights_amendment__sex_blind_reading, 0.68).
domain_priors:suppression_score(equal_rights_amendment__sex_blind_reading, 0.85).
domain_priors:theater_ratio(equal_rights_amendment__sex_blind_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_rights_amendment__sex_blind_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_rights_amendment__sex_blind_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(equal_rights_amendment__sex_blind_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_rights_amendment__sex_blind_reading, snare).
narrative_ontology:human_readable(equal_rights_amendment__sex_blind_reading, "ERA Sex-Blind Reading: Categorical Sex Classification Prohibition").
narrative_ontology:topic_domain(equal_rights_amendment__sex_blind_reading, "constitutional_law/doctrinal_interpretation").

domain_priors:requires_active_enforcement(equal_rights_amendment__sex_blind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_rights_amendment__sex_blind_reading, '82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02').
narrative_ontology:cs_kernel_codification('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', formalized).
narrative_ontology:cs_authority_grounding('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', lineage).
narrative_ontology:cs_interpretation_layer_present('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02').
narrative_ontology:cs_reading_relation('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', equal_rights_amendment__anti_subordination_reading, forecloses).
narrative_ontology:cs_axiom('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', foundational, categorical_sex_classification_forbidden).
narrative_ontology:cs_axiom_status(categorical_sex_classification_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', categorical_sex_classification_forbidden, deontological).
narrative_ontology:cs_axiom('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', foundational, formal_equality_is_constitutional_mandate).
narrative_ontology:cs_axiom_status(formal_equality_is_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', formal_equality_is_constitutional_mandate, deontological).
narrative_ontology:cs_reference_frame('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', formal_equality_constitutional_baseline).
narrative_ontology:cs_drift_state('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', contemporary_remedial_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82b4b4e3-7ccd-4f3c-b4e5-9cf037bf8f02', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(equal_rights_amendment__sex_blind_reading, equal_rights_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_rights_amendment__sex_blind_reading, formal_equality_doctrine_claimants).
narrative_ontology:constraint_victim(equal_rights_amendment__sex_blind_reading, sex_conscious_protective_law).
narrative_ontology:constraint_victim(equal_rights_amendment__sex_blind_reading, remedial_sex_based_classification).
narrative_ontology:constraint_victim(equal_rights_amendment__sex_blind_reading, women_subordination_remediation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX-CONSCIOUS PROTECTIVE LAW (SNARE) — Once this reading is adopted, statutes providing sex-specific protection (pregnancy discrimination law, nursing mother accommodation, domestic violence shelters designed for women) become legally indefensible per se. The law cannot exit without judicial reversal. Full extraction: the reading forbids the very mechanisms that remediate sex-based subordination. No coordination benefit — pure suppression of an alternative approach.
constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BENEFICIARIES OF SEX-CONSCIOUS REMEDIAL LAW (SNARE) — Those who rely on sex-specific protections (pregnant workers, nursing mothers, domestic violence survivors, sexual harassment plaintiffs) face constrained exit: they can reorganize their lives around the loss of protection, but at significant material cost. The reading extracts these protections without alternative provided. Suppression is severe: the constraint forbids acknowledging sex-based harm patterns in law.
constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FORMAL EQUALITY DOCTRINE CLAIMANTS (ROPE) — Institutional actors (judges, legal scholars, civil rights organizations committed to formal equality) experience the sex-blind reading as pure coordination: the reading provides a coherent, mechanical rule that constrains judicial discretion and prevents invidious sex-based classifications. Exit is costless (arbitrage) — they can adopt or abandon the formal equality frame. The reading benefits them by simplifying adjudication and providing doctrinal clarity.
constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-SUBORDINATION-ORIENTED JURISPRUDENCE (TANGLED ROPE) — Legal scholars and advocates committed to dismantling women's subordination face a genuine coordination problem (must articulate a coherent reading of the ERA's mandate) AND asymmetric extraction (this particular reading forecloses their preferred interpretive path). They have some agency — can organize alternative readings, cite state ERAs with different interpretations — but the sex-blind reading, once established in federal doctrine, creates structural pressure against their position. Mixed: coordination obligation + constrained exit.
constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE-LEVEL CONSTITUTIONAL ALTERNATIVES (SCAFFOLD) — States retain the capacity to adopt anti-subordination readings of their own ERAs (Illinois, Pennsylvania, Connecticut have state ERAs) and to ground sex-conscious protective law in state constitutions or statutory frameworks. This is a temporary coordination mechanism with a sunset: federal constitutional doctrine can eventually override state protections, and the supremacy clause constrains the durability of the state-level exit. But in the interim, state constitutions provide organized agents with a genuine alternative.
constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FORMAL EQUALITY AS NATURAL LAW (MOUNTAIN) — From a civilizational/universal analytical perspective, the sex-blind reading appears as a natural law of constitutional interpretation itself: equal protection demands that law make no distinctions on the basis of the very characteristic (sex) that historically justified subordination. Like the principle that law cannot enforce racial caste, formal equality seems immutable. However, the structural data contradicts this classification — the reading has identifiable beneficiaries (formal equality doctrine actors) and clear victims (sex-conscious protective law). The engine's false summit detector will flag this as naturalization of a contested doctrinal reading.
constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_rights_amendment__sex_blind_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_rights_amendment__sex_blind_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_rights_amendment__sex_blind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_rights_amendment__sex_blind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sex-blind reading extracts from sex-conscious protective law by forbidding it entirely. The measurement trajectory (0.45 → 0.62 → 0.68 over 50 years) models how extractiveness accumulates as the doctrine is applied to an expanding domain of sex-specific law: initially applies to explicit classifications (Title IX, military draft exemptions), then extends to proxy classifications (reproductive health law framed as 'capacity-based' rather than sex-based), then to remedial measures (affirmative action in education, workplace accommodation for parental/reproductive roles). Suppression (0.85): Very high. The reading requires complete suppression of one entire class of constitutional argument — the anti-subordination framework. Courts must foreclose, not merely balance, sex-conscious reasoning. Judicial discretion to weigh remedial purposes against formal equality is eliminated. Theater ratio (0.55): Moderate. The reading is doctrinally coherent and mechanically applicable — the theater is not in the reading's internal logic but in the gap between the reading's formal sex-blindness and biological/social realities that sex-consciousness captures. The theater trajectory (0.62 → 0.55 → 0.55) shows that as courts apply the doctrine to edge cases (pregnancy, reproductive capacity, domestic violence), the performative gap between the rule and its effects becomes stable — courts maintain the formal rule while constructing doctrinal exceptions that approach sex-consciousness by proxy. Theater stabilizes because the reading prevents explicit acknowledgment of what the exceptions are achieving.
 *
 * PERSPECTIVAL GAP:
 *   The sex-blind reading instantiates the full range of classification across a single set of base properties. The formal equality claimants (institutional/arbitrage) experience Rope — pure coordination with no extraction. The sex-conscious protective law (powerless/trapped) experiences Snare — maximal extraction with no escape. The beneficiaries of remedial law (moderate/constrained) experience Snare — removal of protections. Anti-subordination jurisprudence (organized/constrained) experiences Tangled Rope — must coordinate on a reading while facing structural pressure against their preferred framework. State constitutional actors (organized/mobile) experience Scaffold — temporary alternative pathway. The analytical observer risks Mountain (natural law) but the structural data reveals this as a false summit — the reading has clear beneficiaries and victims, making it a contingent doctrinal choice, not an immutable principle.
 *
 * DIRECTIONALITY LOGIC:
 *   The sex-blind reading's directionality flows from its beneficiary/victim structure. Formal equality doctrine actors are beneficiaries with arbitrage exit (they can adopt or abandon formal equality framing at low cost) — their derived d is low (~0.15), producing negative/minimal effective extraction chi. Sex-conscious protective law is a victim with trapped exit (once the reading is adopted, the law cannot exit without judicial reversal of the doctrine) — its derived d is high (~0.95), producing maximum effective extraction chi. Beneficiaries of remedial law are moderate victims with constrained exit (they can reorganize life without the protections but at significant cost) — their derived d is moderate-high (~0.75), producing high chi. Anti-subordination jurisprudence is organized with constrained exit (can develop state alternatives but faces structural pressure from federal doctrine) — derived d is moderate (~0.60), producing moderate chi. The engine's chi computation scales the base extractiveness by f(d) and scope σ(S=national=1.0), so the net effective extraction experienced varies dramatically across perspectives despite a stable base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by acknowledging that the sex-blind reading is a CHOICE OF DOCTRINE, not a discovered law of nature. The reading appears to solve the problem 'How do we prevent invidious sex-based discrimination?' by categorical prohibition. But it extracts from sex-conscious remediation by making that remediation illegal. The extraction is not incidental — it is the reading's mechanism. The mandatrophy resolves when we recognize that 'formal equality' (the claimed virtue) and 'sex-conscious remediation is forbidden' (the real effect) are not the same thing. A different reading — the anti-subordination reading — solves the discrimination problem differently: sex classification is permitted when it reduces hierarchy. The two readings are incommensurable within a single framework. The mandatrophy is not 'choose one type' but 'recognize that the choice between readings is a choice between two incompatible extraction mechanisms, not a choice between coordination and extraction.' Both readings extract — they extract from different victim sets (sex-blind extracts from remedial law; anti-subordination reading would extract from formal equality doctrine if adopted). The constraint's classification as Snare is correct FOR THIS READING, from the perspective of those who bear the cost of sex-blindness (remedial law, beneficiaries of sex-conscious protection). A different reading would be a different constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sex_blind_vs_anti_subordination_kernel_contest,
    'Is the ERA''s mandate sex-blindness (formal equality) or anti-subordination (structural inequality remediation)?',
    'Textual analysis (does ''equality'' in ERA mean formal non-classification or substantive non-subordination?); historical intent evidence (suffragist statements on the ERA''s purpose); comparison with race jurisprudence (does strict scrutiny on sex classification follow from race doctrine or create a new framework?)',
    'Sex-blind reading: categorical sex classification forbidden; sex-conscious protective law invalid. Anti-subordination reading: sex classification permitted if it remedies subordination; sex-conscious protective law required. This omega documents that the readings are logically distinct — they cannot coexist in a single legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sex_blind_vs_anti_subordination_kernel_contest, conceptual, 'Fundamental disagreement over the ERA''s constitutional mandate: formal equality vs. anti-subordination').

omega_variable(
    biological_difference_accommodation_paradox,
    'Under sex-blindness, how does law accommodate biological differences (pregnancy, nursing, reproductive capacity) without explicit sex classification?',
    'Doctrinal analysis of proxy classification (can law classify by ''reproductive capacity'' without mentioning sex?); empirical evidence on whether pregnancy-accommodation law must explicitly reference pregnancy to function; comparison with race doctrine (are there race-linked biological differences that strict scrutiny permits to classify?)',
    'If biological differences require sex classification: sex-blindness is impossible in practice; the reading becomes theater (declarative while producing gaps). If proxy classification suffices: sex-blindness is doctrinally coherent but may obscure power asymmetries (framing reproductive capacity as neutral difference).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_difference_accommodation_paradox, empirical, 'Whether sex-blindness can accommodate biological sex differences via proxy classification').

omega_variable(
    reading_establishes_formal_equality_supremacy,
    'Does adopting the sex-blind reading establish formal equality as the constitutional default, making anti-subordination remedies exceptional or impossible?',
    'Doctrinal precedent trajectory: if courts adopt sex-blind reading, do subsequent cases systematically reject sex-conscious remedial law? Do litigants challenging sex-conscious protections cite sex-blind reading as foreclosing the anti-subordination approach?',
    'If yes: the reading functionally encodes formal equality supremacy, making substantive remediation legally unavailable even if new evidence shows sex-conscious measures necessary to reduce inequality. If no: both readings could coexist (formal equality as default, anti-subordination as permissible exception), and the kernel contest remains open.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_establishes_formal_equality_supremacy, empirical, 'Whether sex-blind reading establishes formal equality supremacy and forecloses anti-subordination jurisprudence').

omega_variable(
    extractiveness_measurement_ambiguity,
    'Does the sex-blind reading''s extractiveness (0.68) reflect the reading''s doctrinal content or the reading''s distributional consequence for women relative to men?',
    'Separate extractiveness into: (a) how much the reading constrains legal reasoning (high — forbids one entire class of argument), and (b) how much the reading extracts from beneficiaries of sex-conscious law (high — removes protective mechanisms). If (a) and (b) point in opposite directions, the reading''s classification depends on which extractiveness is salient.',
    'If (a) is primary: sex-blind reading is neutral constraint on reasoning, classified as Rope. If (b) is primary: sex-blind reading is extraction mechanism targeting sex-conscious remediation, classified as Snare. The difference hinges on whether ''extractiveness'' measures constraint severity or distributional harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_measurement_ambiguity, conceptual, 'Ambiguity in how extractiveness is measured for doctrinal readings: reasoning constraint vs. distributional consequence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_rights_amendment__sex_blind_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(era_sb_theater_t0, equal_rights_amendment__sex_blind_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(era_sb_theater_t25, equal_rights_amendment__sex_blind_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(era_sb_theater_t50, equal_rights_amendment__sex_blind_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(era_sb_extract_t0, equal_rights_amendment__sex_blind_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(era_sb_extract_t25, equal_rights_amendment__sex_blind_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(era_sb_extract_t50, equal_rights_amendment__sex_blind_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(era_sb_supp_t0, equal_rights_amendment__sex_blind_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(era_sb_supp_t25, equal_rights_amendment__sex_blind_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(era_sb_supp_t50, equal_rights_amendment__sex_blind_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_rights_amendment__sex_blind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_rights_amendment__sex_blind_reading, equal_rights_amendment__anti_subordination_reading).

% DUAL FORMULATION NOTE:
% The ERA kernel is contested. This story models the sex-blind reading (categorical sex-classification prohibition). The sibling story models the anti-subordination reading (sex classification permitted if it remedies hierarchy). The two readings have different ε values (sex-blind: 0.68 high extraction from remedial law; anti-subordination: would have lower ε because it permits sex-conscious measures). They have opposite victim sets. They foreclose each other — no single legal framework can hold both. They are linked via network.affects_constraints because the adoption of one reading constrains the doctrinal space available to the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
