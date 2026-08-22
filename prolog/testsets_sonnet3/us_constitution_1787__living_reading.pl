% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitution Reading — Evolving Meaning Framework
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the living-constitutionalism reading of the US
 *   Constitution kernel: the position that constitutional meaning
 *   legitimately evolves with social understanding, and that the 1787 text
 *   (plus amendments) functions as an aspirational framework whose
 *   application to concrete cases is properly informed by 'evolving standards
 *   of decency' and contemporary understandings of liberty and dignity.
 *   Landmark applications include Brown v. Board (1954), Griswold (1965), Roe
 *   (1973), Casey (1992), Lawrence (2003), and Obergefell (2015). This is ONE
 *   of three readings of the same kernel (us_constitution_1787): the
 *   originalist_reading (meaning fixed at ratification) and the
 *   positivist_reading (meaning is text plus amendments, judicial
 *   interpretation constrained to text) are separate constraint stories with
 *   their own ε values and structural data — they are not blended into this
 *   one. Under the living reading's own lights, the standing arrangement
 *   under contest is the practice of judicial recognition of evolving
 *   unenumerated rights; ε is authored for that practice as the living
 *   reading itself assesses it, not for a hypothetical rights-respecting
 *   endpoint.
 *
 * KEY AGENTS:
 *   - federal_judicial_elites: administers the reading (institutional/analytical) — decides which claims evolving standards encompass
 *   - unenumerated_rights_claimants: primary beneficiary (moderate/constrained) — obtains protection without needing an amendment
 *   - democratic_legislative_majorities: primary payer (organized/constrained) — loses ordinary policy control over constitutionalized questions
 *   - originalist_litigants: secondary payer (moderate/trapped) — lacks a stable textual target to litigate against
 *   - constitutional_law_academics: analytical observer — traces doctrinal absorption over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.38).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitution Reading — Evolving Meaning Framework").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, 'fdd002b6-7b3c-4731-976e-cf15e2c29938').
narrative_ontology:cs_kernel_codification('fdd002b6-7b3c-4731-976e-cf15e2c29938', fixed_text).
narrative_ontology:cs_authority_grounding('fdd002b6-7b3c-4731-976e-cf15e2c29938', lineage).
narrative_ontology:cs_interpretation_layer_present('fdd002b6-7b3c-4731-976e-cf15e2c29938').
narrative_ontology:cs_reading_relation('fdd002b6-7b3c-4731-976e-cf15e2c29938', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdd002b6-7b3c-4731-976e-cf15e2c29938', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('fdd002b6-7b3c-4731-976e-cf15e2c29938', foundational, constitutional_text_is_aspirational_framework).
narrative_ontology:cs_axiom_status(constitutional_text_is_aspirational_framework, holdable).
narrative_ontology:cs_axiom_grounding('fdd002b6-7b3c-4731-976e-cf15e2c29938', constitutional_text_is_aspirational_framework, conventional).
narrative_ontology:cs_axiom('fdd002b6-7b3c-4731-976e-cf15e2c29938', foundational, judicial_recognition_of_evolving_norms_is_legitimate_interpretive_method).
narrative_ontology:cs_axiom_status(judicial_recognition_of_evolving_norms_is_legitimate_interpretive_method, holdable).
narrative_ontology:cs_axiom_grounding('fdd002b6-7b3c-4731-976e-cf15e2c29938', judicial_recognition_of_evolving_norms_is_legitimate_interpretive_method, instrumental).
narrative_ontology:cs_reference_frame('fdd002b6-7b3c-4731-976e-cf15e2c29938', living_constitutionalism_adaptive_framework).
narrative_ontology:cs_drift_state('fdd002b6-7b3c-4731-976e-cf15e2c29938', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fdd002b6-7b3c-4731-976e-cf15e2c29938', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, judiciary_interpretive_authority).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judicial_elites).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, democratic_legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_litigants).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_level_policy_experimentation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Article III judges, especially appellate and Supreme Court justices, who determine what 'evolving standards of decency' or 'ordered liberty' require in a given era. They administer the reading — deciding which modern claims (privacy, dignity, autonomy) the constitutional text is read to encompass. Their institutional authority expands directly with the scope of what counts as constitutional meaning rather than legislative choice.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judicial_elites, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, federal_judicial_elites, beneficiary).

% Individuals and groups asserting rights not enumerated in the constitutional text — reproductive autonomy, same-sex marriage, digital privacy — who obtain protection through judicial recognition of evolving meaning rather than through the amendment process. They benefit directly when the reading expands but have no guaranteed exit if a later court narrows the same evolving-standards logic.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, unenumerated_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Elected legislative bodies whose policy choices on contested social questions can be displaced when courts read the evolving Constitution to constitutionalize an outcome, removing it from ordinary legislative revision. They retain the formal exit of constitutional amendment, but the supermajority threshold makes that exit largely theoretical for contested social issues.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_legislative_majorities, payer,
    organized, biographical, constrained, national).

% Parties advancing text- and history-based arguments who find courts crediting evolving-standards reasoning over their textual claims. From their position, the living reading substitutes contemporary judicial or elite consensus for adjudicable, backward-looking evidence, leaving them without a stable interpretive target to litigate against.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_litigants, payer,
    moderate, biographical, trapped, national).

% The practice of states serving as laboratories of democracy on contested social questions is curtailed when a national evolving-standards ruling forecloses state-by-state variation. Represented here as a non-agent institutional practice rather than a party, for completeness.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_level_policy_experimentation, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(us_constitution_1787__living_reading, state_level_policy_experimentation).

% Scholars and jurists who would object that 'evolving standards' has no fixed content and that the living reading licenses judges to read their own values into the text. They participate in academic and judicial discourse but rarely control the deciding votes on the courts where the reading is applied.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_legal_scholars, excluded,
    organized, generational, constrained, national).

% Scholars who study how doctrine shifts over time, tracing which social movements' claims get absorbed into constitutional meaning and which do not, without a direct stake in any particular outcome.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_law_academics, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, federal_judicial_elites).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to absorb genuinely new social conditions and moral understandings (privacy in a surveillance age, dignity claims unanticipated in 1787) without requiring the supermajority amendment process for every adaptation, keeping a 18th-century text minimally workable for a vastly changed society.
% TRANSFER_FUNCTION: Moves interpretive authority over contested social questions from elected legislatures and the amendment process to the federal judiciary; moves protection for newly recognized claims to the groups asserting them, at the cost of legislative majorities' ability to set or revise policy on those same questions through ordinary democratic channels.
% ABSENT_VOICES: Originalist legal scholars and textualist judges object that 'evolving standards' imports whatever normative consensus currently prevails among a narrow judicial and academic elite, and that the losing side in a legislative debate can route around democratic loss by recasting its position as a constitutional entitlement — this objection is voiced in dissents and academic literature but does not control outcomes where the reading is applied by a court that has adopted it.
% DISAPPEARANCE_RATIONALE: If the living reading vanished overnight and was replaced by a purely textual or originalist adjudicative standard, a substantial body of recognized but unenumerated rights would lose their present constitutional footing, forcing them back into the ordinary legislative and amendment process; judicial authority over contested social questions would contract sharply, and unresolved questions would revert to state-by-state and legislative resolution.
% FOUNDING_PROBLEM: The constitutional text was drafted for an agrarian, slaveholding, pre-industrial society of roughly four million people and cannot, on its face, resolve questions like digital surveillance, reproductive technology, or same-sex marriage; the living reading was built to let the document remain the operative supreme law without requiring Article V amendment for every social change.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the judiciary and legal academy attest the adaptation problem is permanently live because no constitution can anticipate future social conditions. Originalist judges, textualist scholars, and several state legislatures attest from outside the benefiting judicial seat that the 'gap' is manufactured to justify judicial policymaking, and that the amendment process (used successfully 27 times) remains a live, if harder, alternative — this is a genuinely contested corroboration, not a settled one.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).
:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the living reading genuinely solves a real coordination problem (a fixed 18th-century text applied to unanticipated conditions) and its expansions have often tracked broad social consensus after the fact (Brown, Obergefell), so it is not pure extraction. But it is not costless coordination either — it transfers interpretive authority away from legislatures without their consent and is defended by continuous judicial enforcement (stare decisis, doctrinal elaboration), which is why requires_active_enforcement is true and theater_ratio has crept upward (0.15 to 0.30) as evolving-standards reasoning has become more routinized doctrinal formula than fresh moral inquiry in each case. Suppression (0.38) reflects that legislative majorities and originalist litigants cannot simply exit the arrangement — a losing legislature cannot un-constitutionalize an issue except through the high-threshold amendment process, and a litigant cannot force the court to adjudicate on pure text once evolving-standards doctrine is entrenched. Accessibility_collapse (0.35) and resistance (0.62) reflect that alternatives (textualism, originalism, legislative resolution) remain vigorously live and contested rather than foreclosed — this is a claim held by serious, organized rival positions, not a monopoly.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits nearest the beneficiary end: institutional power, analytical exit (it can revisit or narrow its own doctrine at will), and its authority over contested social questions expands directly with the reading's scope. Unenumerated-rights claimants are beneficiaries but with constrained exit — their gains are contingent on courts continuing to read the Constitution their way, which a later court applying the same living-reading logic could reverse. Democratic legislative majorities and originalist litigants sit toward the target end: organized power in the legislative case, but constrained/trapped exit because the amendment process is a real but practically foreclosed alternative (27 successful amendments in 235+ years against thousands of contested social questions).
 *
 * MANDATROPHY ANALYSIS:
 *   The living reading was built to solve a genuine and still-live problem: a short, 18th-century text cannot mechanically resolve every question a vastly changed society raises, and requiring Article V supermajorities for every adaptation would freeze constitutional meaning in ways almost no serious constitutional theorist, including many originalists, actually wants for structural questions (e.g., the reading extending equal protection to conditions unanticipated by its drafters). Classifying this as tangled_rope rather than snare or rope avoids two mislabeling errors: treating it as a pure snare would ignore genuine, broadly-ratified-in-retrospect coordination gains (Brown); treating it as a pure rope would ignore that legislative majorities bear real, non-consensual costs and that the doctrine requires continuous active judicial enforcement against a live and organized rival reading. The founding_problem_status is authored as contested, not resolved, precisely because whether the adaptation problem remains genuinely live (proponents) or has become a pretext for judicial policymaking (critics) is the actual, unresolved fight between this reading and its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_standards_content_determinacy,
    'Does ''evolving standards of decency'' or ''ordered liberty'' pick out a determinate, judicially discoverable content, or does it function as an empty vessel that judges fill with contemporary elite consensus?',
    'Track whether independently-reasoned applications of the standard across different courts and eras converge on similar outcomes given similar social conditions, versus whether outcomes track the composition of the deciding court more than any external social fact.',
    'If the standard has genuine external content, the living reading functions closer to a rope with real coordination gains; if it primarily tracks the deciding judges'' own values, the coordination story is substantially cover and the reading functions closer to a snare on legislative majorities and rival litigants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evolving_standards_content_determinacy, conceptual, 'Whether evolving-standards doctrine has determinate content or is elite-value substitution.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the living reading selected and sustained because it best captures genuine constitutional meaning, or because it concentrates interpretive authority in the institutional seat (the judiciary) best positioned to declare which reading prevails?',
    'Compare the living reading''s adoption pattern against a control: does the judiciary favor readings that expand judicial authority relative to available alternatives that would constrain it, across a wide sample of doctrinal choices, not just contested social-rights cases?',
    'If adoption correlates with authority expansion independent of substantive merit, this supports treating federal_judicial_elites as a genuine institutional beneficiary rather than a neutral interpreter, strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether the reading is selected on its merits or because it favors the selecting institution''s own authority.').

omega_variable(
    amendment_process_practical_availability,
    'Is the Article V amendment process a genuinely available alternative that legislative majorities decline to use, or is it practically foreclosed by supermajority thresholds regardless of majority preference?',
    'Historical base rate of successful amendments addressing contested social questions versus the frequency such questions are instead resolved judicially; compare against public opinion polling showing majority support for amendment-track resolution that nonetheless failed to produce an amendment.',
    'If amendment is practically foreclosed, the suppression metric (0.38) understates the true lack of exit for legislative majorities, and the classification would move further toward snare; if amendment is a live but disfavored option, the tangled_rope classification with moderate suppression is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_practical_availability, empirical, 'Whether Article V amendment is a real exit option or a formal fiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_1787__living_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_1787__living_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_1787__living_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_1787__living_reading, theater_ratio, 1992, 0.26).
narrative_ontology:measurement(us_c_tr_t2003, us_constitution_1787__living_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_1787__living_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__living_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1954, us_constitution_1787__living_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_1787__living_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_1787__living_reading, base_extractiveness, 1973, 0.35).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_1787__living_reading, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement(us_c_be_t2003, us_constitution_1787__living_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_1787__living_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__living_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1954, us_constitution_1787__living_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_1787__living_reading, suppression_requirement, 1965, 0.24).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_1787__living_reading, suppression_requirement, 1973, 0.3).
narrative_ontology:measurement(us_c_su_t1992, us_constitution_1787__living_reading, suppression_requirement, 1992, 0.33).
narrative_ontology:measurement(us_c_su_t2003, us_constitution_1787__living_reading, suppression_requirement, 2003, 0.35).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_1787__living_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__living_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the us_constitution_1787 kernel, decomposed per the ε-invariance principle because the label 'the Constitution's meaning' conflates structurally distinct interpretive commitments with different beneficiary/victim structures and different ε values. living_reading (this story, ε=0.42, tangled_rope: real coordination with unenumerated-rights adaptation, but non-consensual authority transfer from legislatures) sits alongside originalist_reading (fixed meaning at ratification, different victim set) and positivist_reading (text-plus-amendment, judicial interpretation constrained). All three are linked bidirectionally via affects_constraints since each reading's institutional dominance in a given era structurally affects which claims the sibling readings can plausibly advance in litigation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
