% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Text Authority
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the originalist reading of constitutional
 *   text authority: the claim that constitutional meaning was fixed at
 *   ratification and that interpretive legitimacy derives from recovering the
 *   original public meaning. It is one reading of the contested kernel
 *   constitutional_text_authority, alongside living constitutionalist and
 *   positivist siblings. Key agents by structural relationship: originalist
 *   jurists and the Federalist Society network are the primary beneficiaries
 *   and agenda-setters, empowered by the constraint's demand for historically
 *   grounded argument. Progressive litigants and democratic majority seekers
 *   are the primary targets, trapped in a system where their claims require
 *   eighteenth-century historical pedigrees. Living constitutionalist jurists
 *   and non-originalist academics are payers who must engage the originalist
 *   frame even in dissent, bearing professional and doctrinal costs.
 *
 * KEY AGENTS:
 *   - originalist_jurist: Primary agenda-setter and beneficiary (institutional/constrained) â enforces originalist methodology through judicial opinions and controls interpretive standards.
 *   - federalist_society_network: Primary beneficiary (organized/constrained) â captures institutional influence through nomination pipelines and professional networks.
 *   - constitutional_historians: Secondary beneficiary (moderate/mobile) â provides the historical evidence gatekeeping resource.
 *   - progressive_litigant: Primary target (powerless/trapped) â bears extraction through higher barriers to constitutional recognition.
 *   - living_constitutionalist_jurist: Secondary target (institutional/constrained) â interpretive autonomy constrained by mandatory engagement with originalist frames.
 *   - non_originalist_legal_academic: Payer/excluded voice (moderate/constrained) â marginalized in elite legal institutions.
 *   - democratic_majority_seekers: Excluded target (powerless/trapped) â locked out of constitutional adaptation without Article V supermajorities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.65).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.72).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'e507d927-5bd7-48eb-b1ed-a813402f0891').
narrative_ontology:cs_kernel_codification('e507d927-5bd7-48eb-b1ed-a813402f0891', fixed_text).
narrative_ontology:cs_authority_grounding('e507d927-5bd7-48eb-b1ed-a813402f0891', lineage).
narrative_ontology:cs_interpretation_layer_present('e507d927-5bd7-48eb-b1ed-a813402f0891').
narrative_ontology:cs_reading_relation('e507d927-5bd7-48eb-b1ed-a813402f0891', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e507d927-5bd7-48eb-b1ed-a813402f0891', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('e507d927-5bd7-48eb-b1ed-a813402f0891', foundational, original_public_meaning_binds).
narrative_ontology:cs_axiom_status(original_public_meaning_binds, holdable).
narrative_ontology:cs_axiom_grounding('e507d927-5bd7-48eb-b1ed-a813402f0891', original_public_meaning_binds, conventional).
narrative_ontology:cs_reference_frame('e507d927-5bd7-48eb-b1ed-a813402f0891', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('e507d927-5bd7-48eb-b1ed-a813402f0891', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e507d927-5bd7-48eb-b1ed-a813402f0891', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_jurist).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, federalist_society_network).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, constitutional_historians).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_litigant).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_jurist).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, non_originalist_legal_academic).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, democratic_majority_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution according to original public meaning and enforces this methodology through judicial opinions that delegitimize non-originalist arguments; professional advancement and appointment depend on originalist credentials.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_jurist, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, originalist_jurist, beneficiary).

% Promotes originalist jurisprudence through judicial nomination pipelines, law school chapters, and professional networks; captures institutional influence by making originalism the prerequisite for elite legal appointments.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, federalist_society_network, beneficiary,
    organized, generational, constrained, national).

% Supply the archival research and historical narratives that originalist interpretation demands; their professional expertise becomes a gatekeeping resource that determines which constitutional arguments are institutionally legitimate.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_historians, beneficiary,
    moderate, biographical, mobile, national).

% Seeks constitutional recognition for rights not explicitly enumerated or demonstrable in eighteenth-century public meaning (privacy, reproductive autonomy, LGBTQ equality); faces systematically higher doctrinal barriers under originalist frameworks.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, progressive_litigant, payer,
    powerless, immediate, trapped, national).

% Must argue within originalist methodological frames even when reaching non-originalist outcomes; interpretive autonomy is constrained by the need to respond to historical-evidence challenges from originalist colleagues.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_jurist, payer,
    institutional, biographical, constrained, national).

% Publishes methodological critiques of originalism but is increasingly marginalized in elite appellate practice and Supreme Court clerk hiring; career opportunities narrow as originalism becomes a prerequisite for elite legal positions.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, non_originalist_legal_academic, payer,
    moderate, biographical, constrained, national).

% Would seek constitutional adaptation through ordinary democratic processes rather than Article V supermajorities; structurally excluded from altering constitutional meaning under the originalist framework, which locks in eighteenth-century arrangements against contemporary majority preferences.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, democratic_majority_seekers, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically anchored method for interpreting constitutional text across time, preventing judicial discretion from collapsing into mere policy preference and supplying a common framework for constitutional argument.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary moral reasoning and democratic majorities to historical evidence and the scholars and jurists who control its excavation; moves constitutional protection away from unenumerated rights claimants toward textually explicit or historically demonstrable liberties.
% ABSENT_VOICES: Non-originalist legal academics and democratic majorities seeking constitutional adaptation without Article V supermajorities are structurally absent from the authoritative interpretive conversation; their methodological objections are treated as illegitimate from the outset.
% DISAPPEARANCE_RATIONALE: If the fixed-at-ratification constraint vanished, constitutional interpretation would shift to contemporary moral reasoning or democratic preferences, unenumerated rights would become easier to recognize, and the current ecosystem of originalist scholarship, Federalist Society networks, and historical-evidence gatekeeping would lose its institutional centrality.
% FOUNDING_PROBLEM: How to constrain unelected judges from imposing personal policy preferences under the guise of constitutional interpretation, and how to legitimate judicial review in a democratic system.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and the Federalist Society attest the problem is still live, citing Lochner-era judicial activism. Non-originalist scholars and democratic theorists attest the problem has shifted: contemporary originalism itself functions as a mechanism for judicial constraint of democratic majorities, and the original interpretive problem has been superseded by the problem of constitutional obsolescence; this corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-19',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the constraint empowers a methodological elite to determine constitutional outcomes through control of historical evidence, while systematically blocking rights claims that lack ratification-era pedigrees. Suppression (0.72) is higher than extractiveness because the constraint's persistence depends on active enforcement: judicial appointments that screen for originalism, law school hiring that rewards originalist credentials, and professional norms that delegitimize non-originalist interpretation. Theater ratio (0.45) reflects significant performative maintenance â the frame of objective historical discovery obscures the methodological choices and political consequences embedded in originalist practice. Accessibility collapse (0.60) is moderate-high: alternatives (living constitutionalism, moral reasoning, democratic updating) are institutionally delegitimized but not extinguished. Resistance (0.55) reflects sustained opposition from legal academia and progressive jurisprudence, though originalism has gained substantial institutional dominance over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The originalist jurist seat experiences this constraint as a Mountain-of-meaning â a discovered historical fact that judges merely apply. The progressive litigant seat experiences it as a Snare â a constructed apparatus that extracts constitutional protection from vulnerable groups. The structural truth is Tangled Rope: there is a genuine coordination function (preventing arbitrary judicial rule) but the same structure that coordinates also extracts asymmetrically, concentrating interpretive power in a historically oriented elite while displacing democratic adaptation. The engine computes this divergence from the structural data; the authored claim does not adjudicate the seat-level perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist jurists and the Federalist Society network are positioned near the beneficiary pole (low d): they collect institutional authority and nomination control from the constraint's operation. Constitutional historians are also beneficiaries but with mobile exit, so their d is slightly higher (~0.25) than the fully constrained jurists (~0.15). Progressive litigants are trapped in the constitutional system and bear direct costs, placing them near full target (d ~0.95). Living constitutionalist jurists are institutionally powerful but methodologically constrained, giving them a high but not maximal d (~0.80). Non-originalist academics and democratic majority seekers are excluded or marginalized, placing them in the high-d range (~0.85â0.90). The victim and beneficiary declarations drive this derivation; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â constraining unelected judges â is contested as still live. The Tangled Rope classification prevents mislabeling by capturing both the genuine coordination value (textual stability, democratic legitimacy through enacted meaning) and the asymmetric extraction (empowering an originalist elite, constraining democratic adaptation). If the coordination story were pure cover, the constraint would be a Snare; if there were no extraction, it would be a Rope. The temporal measurements show rising extractiveness and suppression over the interval, suggesting that whatever genuine coordination existed at the founding has been supplemented by institutional rent-seeking. The mandatrophy is unresolved: the arrangement persists though its original problem may have shifted from judicial tyranny to constitutional obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_empirical_recoverability,
    'Is the original public meaning of eighteenth-century constitutional provisions empirically recoverable, or does the historical record underdetermine the answers to contemporary constitutional questions?',
    'Systematic meta-analysis of originalist historical claims against archival evidence, evaluating whether multiple coherent original meanings exist for contested provisions.',
    'If original meaning is irrecoverable or multiply realizable, the constraint functions as a grant of methodological discretion to originalist elites rather than a genuine historical limit, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_empirical_recoverability, empirical, 'Whether historical evidence can actually fix constitutional meaning.').

omega_variable(
    kernel_reading_inversion,
    'If the living constitutionalist reading were adopted as the operative constraint, would the directionality map invert such that today''s beneficiaries become targets and vice versa?',
    'Comparative analysis of the same institutional actors under a living constitutionalist regime, tracking judicial appointment politics and academic gatekeeping patterns.',
    'If directionality inverts completely, the extraction is primarily a property of the interpretive monopoly rather than the specific methodology, suggesting the kernel itself is a contested power structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_inversion, conceptual, 'Whether the constraint''s extractiveness is methodology-specific or kernel-structural.').

omega_variable(
    founding_problem_mandatrophy,
    'Has the founding problem of judicial tyranny been solved by originalism, or has originalism itself become a new form of judicial constraint on democratic majorities?',
    'Empirical study of originalist judicial decisions striking down democratic legislation compared to non-originalist decisions; tracking whether originalism constrains judges or empowers them against legislatures.',
    'If originalism does not demonstrably constrain judicial discretion more than alternatives, the coordination story is cover and classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_mandatrophy, empirical, 'Whether originalism solves or perpetuates the judicial discretion problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cta_orig_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cta_orig_tr_t8, constitutional_text_authority__originalist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(cta_orig_tr_t16, constitutional_text_authority__originalist_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(cta_orig_tr_t24, constitutional_text_authority__originalist_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cta_orig_tr_t32, constitutional_text_authority__originalist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(cta_orig_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(cta_orig_tr_t44, constitutional_text_authority__originalist_reading, theater_ratio, 44, 0.45).

% Extraction over time
narrative_ontology:measurement(cta_orig_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cta_orig_be_t8, constitutional_text_authority__originalist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(cta_orig_be_t16, constitutional_text_authority__originalist_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(cta_orig_be_t24, constitutional_text_authority__originalist_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(cta_orig_be_t32, constitutional_text_authority__originalist_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(cta_orig_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(cta_orig_be_t44, constitutional_text_authority__originalist_reading, base_extractiveness, 44, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cta_orig_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cta_orig_su_t8, constitutional_text_authority__originalist_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(cta_orig_su_t16, constitutional_text_authority__originalist_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(cta_orig_su_t24, constitutional_text_authority__originalist_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(cta_orig_su_t32, constitutional_text_authority__originalist_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(cta_orig_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cta_orig_su_t44, constitutional_text_authority__originalist_reading, suppression_requirement, 44, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% The kernel constitutional_text_authority decomposes into at least three structurally distinct constraints: originalist_reading (fixed historical meaning), living_constitutionalist_reading (evolving social meaning), and positivist_reading (formal enactment validity). Each reading has different epsilon values, beneficiary/victim structures, and directionality maps. They are linked as a constraint family through mutual institutional coupling in the federal judiciary and legal academy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
