% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Reading of Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the living-constitution reading of the contested
 *   US constitutional interpretive kernel: the claim that constitutional
 *   meaning legitimately evolves with societal values, and that interpretive
 *   authority derives from courts' reasoned adaptation of text to
 *   contemporary conditions rather than from fidelity to a fixed original
 *   meaning. This reading is what produced doctrines like substantive due
 *   process privacy rights, an expansive post-New Deal Commerce Clause, and
 *   modern equal protection jurisprudence recognizing claims the ratifying
 *   generations did not contemplate. It genuinely solves a coordination
 *   problem (Article V amendment is nearly impossible to invoke) but does so
 *   by transferring interpretive authority to an institution — the federal
 *   judiciary — whose composition determines whose claims get textual cover,
 *   producing real winners and real losers along partisan and ideological
 *   lines that shift with judicial appointments.
 *
 * KEY AGENTS:
 *   - civil_rights_expansion_claimants: primary beneficiary (moderate/constrained) — depends on evolving-meaning doctrine for legal standing
 *   - federal_judiciary: agenda_setter (institutional/analytical) — administers and enforces the reading case by case
 *   - states_rights_advocates: primary payer (organized/constrained) — loses regulatory autonomy to expansive federal doctrine
 *   - original_meaning_textualists: primary payer (organized/constrained) — loses interpretive legitimacy and precedential ground
 *   - constitutional_law_scholars: analytical observer — theorizes and evaluates competing readings without binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.42).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.38).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Reading of Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, 'ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3').
narrative_ontology:cs_kernel_codification('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', fixed_text).
narrative_ontology:cs_authority_grounding('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', practice).
narrative_ontology:cs_interpretation_layer_present('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3').
narrative_ontology:cs_reading_relation('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', us_constitution_interpretive__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', foundational, constitutional_meaning_evolves_with_societal_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_societal_values, holdable).
narrative_ontology:cs_axiom_grounding('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', constitutional_meaning_evolves_with_societal_values, conventional).
narrative_ontology:cs_axiom('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', foundational, judicial_reasoned_elaboration_is_legitimate_interpretive_method).
narrative_ontology:cs_axiom_status(judicial_reasoned_elaboration_is_legitimate_interpretive_method, holdable).
narrative_ontology:cs_axiom_grounding('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', judicial_reasoned_elaboration_is_legitimate_interpretive_method, instrumental).
narrative_ontology:cs_reference_frame('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', post_new_deal_adaptive_jurisprudence).
narrative_ontology:cs_drift_state('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ba76deb9-0b9d-47a5-b693-6bfcadcc9cb3', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, entities_constrained_by_expanded_federal_reach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on courts reading equal protection and due process as expanding to cover conditions unforeseen at ratification. Their claims to standing and remedy depend on judges treating constitutional text as capable of new application to new social facts; without that reading many of their legal victories have no textual anchor.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    moderate, generational, constrained, national).

% Depend on unenumerated-rights doctrine (privacy, bodily autonomy) derived from evolving interpretation rather than explicit text. Their legal position is directly exposed to reversal when interpretive authority shifts toward original-meaning readings, as demonstrated by post-hoc doctrinal reversal.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, constrained, national).

% Secured marriage equality and anti-discrimination protections through living-constitution reasoning about dignity and equal liberty absent from 1868 or 1791 original understanding. Their legal gains have no purchase under a strict originalist framework and remain contingent on which reading currently commands judicial majorities.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, generational, constrained, national).

% Administer expansive regulatory programs (environmental, labor, financial, health) whose constitutional basis rests on a broadly read Commerce Clause and implied federal powers. They both benefit from and actively press for interpretive frameworks that sustain this reach, filing briefs and shaping doctrine through litigation strategy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies, agenda_setter).

% Article III courts, particularly the Supreme Court, administer this reading by deciding which contemporary conditions and societal values justify departure from historical textual meaning. They set the terms of adaptation case by case and are the enforcement mechanism through which the reading persists or is displaced.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% State legislatures and governors whose regulatory autonomy is displaced when federal courts read Commerce Clause and Fourteenth Amendment power expansively. They can litigate, lobby for constitutional amendment, or attempt nullification-adjacent resistance, but cannot exit the federal system; their remedy is slow and structurally disadvantaged against a doctrine that grows with each precedent.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    organized, generational, constrained, regional).

% Judges, scholars, and litigants committed to fixed original meaning experience each living-constitution decision as delegitimizing their interpretive method itself, not merely losing a case. Their remedy is judicial appointment cycles and doctrinal argument over decades; they cannot exit the constitutional system to escape the reading's dominance when it holds a judicial majority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, civilizational, constrained, national).

% Businesses, local governments, and individuals subject to federal regulation justified by expansive Commerce Clause and implied-powers reasoning bear compliance costs and reduced local autonomy they would not bear under a narrower reading. Their exit options are limited to relocation across jurisdictions offering only marginal relief, since the federal reach is national in scope.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, entities_constrained_by_expanded_federal_reach, payer,
    moderate, biographical, constrained, national).

% Study and theorize the competing interpretive methods, produce the doctrinal frameworks (living constitutionalism, originalism, popular constitutionalism) that judges cite, and assess which reading is ascendant at a given historical moment without themselves holding binding authority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional text written for an 18th/19th-century agrarian, slaveholding, pre-industrial society to remain applicable to a technologically transformed, pluralistic, industrial and post-industrial polity without requiring formal amendment for every social change — solving the genuine problem that Article V amendment is extraordinarily difficult to achieve.
% TRANSFER_FUNCTION: Moves interpretive and regulatory authority from state legislatures and from the fixed original textual meaning toward federal courts and federal agencies; moves legal recognition and protection toward groups whose claims lack explicit textual grounding (privacy, dignity, equal liberty claimants) and away from those who would prevail under a text-constrained reading.
% ABSENT_VOICES: Framers and ratifying publics of 1787/1791/1868 cannot testify to whether they intended their text to be read this way; state legislatures displaced by federal expansion often lack standing or practical means to contest doctrine case-by-case; the general public rarely participates directly in the interpretive contest, which plays out among judges, litigants, and scholars.
% DISAPPEARANCE_RATIONALE: If living-constitution interpretive authority disappeared overnight in favor of strict original meaning, unenumerated rights doctrines (privacy-based reproductive rights, substantive due process protections for intimate relationships, much modern equal protection doctrine) would lose their doctrinal foundation, federal regulatory programs resting on expansive Commerce Clause readings would face constitutional challenge, and decades of precedent would become vulnerable to reversal — a substantial rearrangement of law and policy across multiple domains.
% FOUNDING_PROBLEM: The Constitution's amendment process (Article V) is so demanding that formal textual change cannot keep pace with social, technological, and moral change; courts needed an interpretive method allowing constitutional law to address conditions the framers could not have anticipated (industrialization, incorporation of the Bill of Rights against states, civil rights, technology, evolving understandings of equality and privacy) without waiting for supermajority amendment.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the beneficiary groups (including scholars sympathetic to originalism) corroborate that Article V's amendment difficulty is real and that judicial adaptation has functioned as a substitute amendment mechanism across the 20th century. Originalist scholars corroborate the same historical pattern but characterize it as an illegitimate workaround rather than a solution to a genuine problem, so the status of the founding problem is disputed rather than settled even though its existence is not.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).
:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a substantial but not extreme transfer: federal courts genuinely resolve some real amendment-process bottleneck (coordination function present), but the same mechanism systematically reallocates interpretive authority and legal protection along lines that track which claimants can construct persuasive contemporary-values arguments before a judiciary of a given ideological composition. Suppression (0.38) is moderate — states and textualists are not silenced, they retain full political and litigation voice, but the doctrine structurally disadvantages them because reversing an established living-constitution precedent requires overturning stare decisis rather than merely winning an election. Resistance is high (0.68) because this reading is one of the most contested claims in American law, actively opposed by a well-organized originalist legal movement with its own judicial philosophy, law schools, and appointment strategy. Accessibility collapse is moderate (0.35): alternative interpretive methods (originalism, popular constitutionalism) remain fully articulated and politically viable, unlike a genuine natural-law constraint where alternatives disappear once understood.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil rights, reproductive autonomy, and LGBTQ+ rights claimants derive d toward the beneficiary end because their legal recognition is substantively created or secured by this interpretive method; without it their claims have no textual home. Federal regulatory agencies sit similarly close to the beneficiary end, and additionally hold agenda-setting power because agencies both rely on and litigate to preserve expansive Commerce Clause readings. States' rights advocates and original-meaning textualists sit near the target end: their political and doctrinal position is structurally disadvantaged by a reading that treats their preferred fixed-meaning framework as illegitimate. Entities constrained by expanded federal reach are moderate targets — real compliance costs, but not identity-level stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Article V's near-impossibility) is genuinely still live — amendment remains extraordinarily rare — which argues against treating this reading as a pure zombie mandate. But its status is marked contested rather than dead or live cleanly, because originalists corroborate the historical bottleneck exists while disputing that judicial adaptation is a legitimate remedy for it rather than an unaccountable substitute for the amendment process the framers deliberately made difficult. This mismatch — problem genuinely live, but legitimacy of the chosen remedy sharply contested — is exactly the structure the tangled_rope classification is built to hold: real coordination function, real asymmetric cost, both simultaneously true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_vs_authority_transfer,
    'Is judicial adaptation of constitutional text to contemporary conditions a legitimate exercise of interpretive authority inherent in the judicial role, or is it an unaccountable transfer of amendment-level lawmaking power from the constitutionally prescribed Article V process to unelected judges?',
    'No empirical resolution exists; this is a foundational question of constitutional theory that different legal traditions answer differently based on priors about democratic legitimacy, judicial role, and the nature of law. Could be informed by comparative study of how other constitutional democracies with more amendable texts handle analogous adaptation pressures.',
    'If judicial adaptation is legitimate interpretive authority, this reading is closer to a rope (genuine coordination solving a real structural problem with judges as trustees). If it is an unaccountable transfer, the same structure is closer to a snare wearing coordination language as cover, with courts as the extracting agent and constitutional text as pretext.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_vs_authority_transfer, conceptual, 'Whether living constitutionalism is legitimate adaptation or judicial rent-seeking dressed as interpretation.').

omega_variable(
    reversibility_asymmetry,
    'Given that living-constitution doctrines can themselves be reversed by a differently composed judiciary (as demonstrated historically), is the extraction this reading produces durable, or is it merely contingent on current judicial composition and therefore better modeled as a pendulum than a settled transfer?',
    'Longitudinal tracking of doctrinal reversal rates and the political conditions (appointment cycles, confirmation battles) that produce them, compared against the reversal rate of amendment-based constitutional change.',
    'If reversal risk is high and structurally built into the appointment process, the effective extraction experienced by any beneficiary group is lower than the static metric suggests, because gains are contingent rather than locked in — this argues for treating beneficiary status as time-horizon-dependent rather than fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_asymmetry, empirical, 'Whether living-constitution gains are durable transfers or contingent, reversible pendulum swings tied to judicial composition.').

omega_variable(
    founding_problem_genuine_vs_pretextual,
    'Is the Article V amendment bottleneck the actual reason living constitutionalism arose, or is it a post-hoc justification for judicial preferences that would have expanded interpretive authority regardless of amendment difficulty?',
    'Historical analysis of whether living-constitution reasoning tracks periods of acute amendment failure specifically, or instead tracks periods of judicial ideological realignment independent of amendment attempts.',
    'If genuinely tied to amendment bottleneck, the coordination function is real and substantial. If pretextual, the coordination story is closer to cover for a court simply exercising preferred policy judgment, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_genuine_vs_pretextual, empirical, 'Whether the Article V bottleneck genuinely explains living constitutionalism''s emergence or merely rationalizes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__living_constitution_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__living_constitution_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_interpretive__living_constitution_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t80, observed).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_interpretive__living_constitution_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t80, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement_basis(us_c_be_t80, observed).
narrative_ontology:measurement(us_c_be_t100, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(us_c_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t80, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 80, 0.36).
narrative_ontology:measurement_basis(us_c_su_t80, observed).
narrative_ontology:measurement(us_c_su_t100, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement_basis(us_c_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__living_constitution_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the us_constitution_interpretive kernel. living_constitution_reading claims broad judicial adaptive authority (ε=0.42, tangled_rope); originalist_reading claims fixed original meaning as the sole legitimate interpretive method (separate file, distinct beneficiary/victim structure inverting this one's); popular_constitutionalism_reading relocates interpretive authority to political movements rather than courts (separate file). The three do not share ε — each instantiates a structurally distinct claim about where interpretive authority sits and who it favors. Network edges here mark structural rivalry/influence, not shared measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
