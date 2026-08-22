% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Reading: Constitutional Meaning Fixed at Ratification
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint models the originalist reading of the contested US
 *   constitutional meaning kernel: the claim that constitutional text has a
 *   fixed, historically ascertainable public meaning fixed at ratification
 *   (or, for amendments, at the amendment date), and that judges are bound to
 *   apply that historical meaning regardless of contemporary social
 *   evolution. This is generated as ONE clean reading among three siblings
 *   (living_constitutionalist_reading, positivist_reading), each instantiated
 *   as a separate constraint per the eps-invariance principle. The
 *   originalist reading's ratification-fixed meaning premise, its treatment
 *   of historical evidence as dispositive, and its systematic exclusion of
 *   contemporary circumstance from the meaning inquiry (though not from
 *   application) are all specific to this reading and are not shared by the
 *   sibling constraints, which have their own eps values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Reading: Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '5a002dfb-0b05-4e24-89de-e193c8ffa9b4').
narrative_ontology:cs_kernel_codification('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', fixed_text).
narrative_ontology:cs_authority_grounding('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', lineage).
narrative_ontology:cs_interpretation_layer_present('5a002dfb-0b05-4e24-89de-e193c8ffa9b4').
narrative_ontology:cs_reading_relation('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', secondary, contemporary_circumstance_irrelevant_to_meaning).
narrative_ontology:cs_axiom_status(contemporary_circumstance_irrelevant_to_meaning, holdable).
narrative_ontology:cs_axiom_grounding('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', contemporary_circumstance_irrelevant_to_meaning, instrumental).
narrative_ontology:cs_reference_frame('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', founding_era_public_meaning_fixation).
narrative_ontology:cs_drift_state('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', contemporary_pluralist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5a002dfb-0b05-4e24-89de-e193c8ffa9b4', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judiciary_appointees).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, settled_property_and_contract_interests).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, groups_excluded_from_1788_franchise).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, modern_regulatory_reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges committed to public-meaning originalism decide cases by researching ratification-era dictionaries, debates, and legal practice, treating that historical record as dispositive of constitutional meaning. They administer the interpretive method, select which historical sources count as evidence, and can expand or narrow its application through opinion-writing. Their institutional standing and reputational capital are built on methodological consistency.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judiciary_appointees, agenda_setter,
    institutional, generational, arbitrage, national).

% Legal scholars, advocacy organizations, and political coalitions who favor binding judicial discretion to historical text benefit from a method that predictably resists majoritarian legislative and social change. They gain a durable rhetorical and litigation framework that can be invoked regardless of which party controls elected branches, and they face no direct cost from the arrangement's operation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Holders of vested economic arrangements benefit when constitutional meaning is locked against reinterpretation that might redistribute entitlements; the fixed-meaning method insulates existing property, contract, and structural-power arrangements from claims grounded in evolving social consensus.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, settled_property_and_contract_interests, beneficiary,
    powerful, civilizational, arbitrage, national).

% Individuals asserting rights not clearly rooted in 1788 or amendment-era historical practice (reproductive autonomy, certain privacy and equality claims) find courts require them to produce historical evidence that, by the nature of their claim, often cannot exist. They have no exit from the federal constitutional system and must litigate within a framework structurally weighted against claims lacking period-specific pedigree.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Descendants of populations who had no voice in ratification-era lawmaking — enslaved people, women, non-property-holders — bear a structural asymmetry: the historical record the method privileges was authored without their participation, so claims resting on their interests are least likely to find supporting period evidence. They cannot exit the interpretive regime and cannot retroactively supply the missing historical voice.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, groups_excluded_from_1788_franchise, payer,
    powerless, civilizational, trapped, national).

% Movements seeking administrative and regulatory innovation (environmental, financial, public health) encounter originalist doctrine as a constraint on federal power grounded in period-specific understandings of enumerated powers. They can lobby for legislation or constitutional amendment, but amendment is prohibitively difficult, so their practical exit is confined to working within doctrine or awaiting judicial composition change.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, modern_regulatory_reform_movements, payer,
    organized, generational, constrained, national).

% Judges who would read constitutional principles as enduring but their application as evolving are present on the bench but structurally delegitimized within originalist doctrinal discourse as departing from 'proper' interpretive method; their competing account of legitimate constitutional reasoning is treated as illegitimate departure rather than an equally available reading.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, constrained, national).

% Academics analyze the historical, doctrinal, and political consequences of the originalist method, producing empirical studies of how consistently historical evidence is applied and whether the method's claimed neutrality matches its actual outcomes across ideological lines.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, precedent-resistant decision rule that constrains judicial discretion by tying constitutional meaning to a fixed historical referent, reducing the risk that individual judges substitute personal policy preferences for law and offering litigants a stable target of legal argument.
% TRANSFER_FUNCTION: Moves interpretive authority from evolving democratic and social consensus toward the historical record as curated and applied by the current judiciary; moves practical constitutional protection away from claimants whose interests were unrepresented at ratification and toward interests already entrenched at that historical moment.
% ABSENT_VOICES: The populations excluded from 1788-era lawmaking — enslaved people, women, non-property-holding men — have no direct textual or historical voice the method can consult, so their absence from the founding record becomes, within the method, evidence against contemporary claims grounded in their interests; they are structurally unable to supply the missing pedigree.
% DISAPPEARANCE_RATIONALE: If originalism disappeared as the dominant interpretive method overnight, cases currently decided by historical-meaning analysis would be decided by purposivist or living-constitutionalist reasoning instead, likely expanding protection for claims currently rejected for lack of historical pedigree; originalists dispute that this would be an improvement, arguing it would remove a check on judicial policymaking. Whether the world 'rearranges' depends on which reading one holds, which is exactly the kernel contest.
% FOUNDING_PROBLEM: The method was substantially systematized in the late twentieth century to solve a perceived problem of unconstrained judicial discretion — the concern that judges applying evolving constitutional principles were, in practice, substituting contemporary policy preferences for law, undermining democratic legitimacy and predictability.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and scholars attest the discretion-constraint problem remains live, citing continued disagreement over unenumerated rights doctrine. Legal historians and comparative-method scholars outside the originalist camp attest that empirical studies of judicial voting patterns show originalist and non-originalist judges reach outcomes correlated with contemporary political commitments at similar rates, suggesting the discretion-elimination function is substantially unrealized in practice even where the method is professed.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the method functions as genuine coordination (predictability, discretion constraint) for some litigants while systematically disadvantaging claimants whose interests have no ratification-era historical analogue -- a structural asymmetry rooted in who was excluded from the 1788 and amendment-era political process. Suppression (0.72) is high and rising over the measured interval because the method's authority depends on treating non-originalist reasoning as methodologically illegitimate rather than as a competing, equally available interpretive choice -- this delegitimization is an active suppression mechanism, not a passive byproduct. Accessibility collapse (0.62) is substantial: once a court commits to the historical-meaning inquiry, non-historical arguments for the same right become doctrinally foreclosed within that court's reasoning, though the political process (amendment, legislation, future doctrinal shift) remains theoretically open. Resistance (0.70) is high, tracking sustained academic, judicial, and political contestation of the method's neutrality claims.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (originalist judiciary), the method appears as principled discretion-constraint -- a rope. From the payer seats (unenumerated rights claimants, excluded-franchise-descendant groups), the same structure operates as an enforced historical veto that structurally cannot be satisfied by claims resting on the interests of populations absent from the ratification record -- a tangled rope shading toward snare. The engine computes these divergent seat-level readings from the structural data; the claimed_type here names the tangled_rope structure this reading holds to be true.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary appointees function as the agenda-setting seat: they administer the method and their institutional legitimacy depends on its consistent application. Counter-majoritarian constraint advocates and settled property/contract interests are beneficiaries -- they gain a durable structural protection against redistributive or expansive-rights change without bearing the costs of foreclosed claims. Unenumerated rights claimants, groups excluded from the 1788 franchise, and modern regulatory reform movements are targets: their claims are structurally disadvantaged by a method that treats absence of period-specific historical support as evidence against constitutional protection, and they have trapped or constrained exit because the federal constitutional system is not one they can leave.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genuine contested genealogy: originalism was substantially systematized to solve unconstrained judicial discretion, and whether that problem remains live is disputed by parties outside the originalist camp who present empirical voting-pattern evidence suggesting the discretion-elimination function is not delivered in practice. This is exactly the kind of divergence the mandatrophy analysis exists to flag -- a coordination justification (discretion constraint) persisting alongside evidence that the mechanism does not perform the function it is justified by, without collapsing the story into simple bad-faith extraction. The classification (tangled_rope) holds both the genuine coordination function and the asymmetric extraction simultaneously rather than forcing a choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_reading_disagreement_location,
    'Where exactly does the originalist reading''s core premise diverge from the living-constitutionalist and positivist readings, structurally?',
    'Compare the three readings'' treatment of a single case (e.g., an equal protection claim): originalism asks what 1868 public meaning of ''equal protection'' would have permitted; living-constitutionalism asks what the enduring principle requires given contemporary understanding; positivism asks only whether the amendment was validly enacted and treats the content question as separate from validity. The disagreement is located in the MEANING-FIXING moment, not in whether the Constitution binds.',
    'This is committer structure, not a metric dispute -- it establishes that the three readings are genuinely different constraints with different victim sets (originalism disadvantages claims lacking period pedigree; living-constitutionalism disadvantages claims that cannot be framed as evolving-principle application; positivism is agnostic on both and instead advantages whoever controls the formal enactment process).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_reading_disagreement_location, conceptual, 'Locating where the three kernel readings structurally diverge.').

omega_variable(
    historical_meaning_determinacy,
    'Is 1788/amendment-era public meaning genuinely ascertainable with enough determinacy to bind outcomes, or does the historical record''s incompleteness give originalist judges effectively unconstrained discretion dressed as historical inquiry?',
    'Empirical study comparing originalist judges'' historical-meaning conclusions against blind historian panels'' independent conclusions on the same evidence; convergence would support determinacy, systematic divergence correlated with judges'' other commitments would support the discretion-in-disguise hypothesis.',
    'If historical meaning is genuinely determinate, the coordination function (discretion constraint) is real and extraction is lower than authored. If indeterminate, the method''s suppression of non-originalist reasoning is unsupported by its own justification, and the classification should move further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Whether ratification-era public meaning is determinate enough to constrain, or merely to legitimate, judicial discretion.').

omega_variable(
    counter_majoritarian_beneficiary_scope,
    'Do counter-majoritarian constraint advocates benefit from originalism specifically, or from judicial restraint generally -- would they equally favor a strong stare decisis or textualist-without-history method if it produced similar outcomes?',
    'Survey advocacy positions across cases where originalist and non-originalist-restraint methods would diverge in outcome; consistent preference for originalist outcomes over restraint-generally outcomes would indicate the beneficiary relationship is to originalism''s specific substantive results, not to the discretion-constraint coordination function per se.',
    'If beneficiaries favor outcomes over method, the coordination story is weaker cover for outcome-selection, pushing the classification toward snare; if they favor method consistently even against their outcome preferences, the coordination function is more genuine, supporting tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_beneficiary_scope, conceptual, 'Whether the declared beneficiary group is attached to the method or to its typical outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_meaning__originalist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(us_c_tr_t1989, us_constitution_meaning__originalist_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(us_c_tr_t1998, us_constitution_meaning__originalist_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(us_c_tr_t2007, us_constitution_meaning__originalist_reading, theater_ratio, 2007, 0.23).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_meaning__originalist_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_meaning__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_meaning__originalist_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(us_c_be_t1989, us_constitution_meaning__originalist_reading, base_extractiveness, 1989, 0.4).
narrative_ontology:measurement(us_c_be_t1998, us_constitution_meaning__originalist_reading, base_extractiveness, 1998, 0.45).
narrative_ontology:measurement(us_c_be_t2007, us_constitution_meaning__originalist_reading, base_extractiveness, 2007, 0.5).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_meaning__originalist_reading, base_extractiveness, 2016, 0.53).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_meaning__originalist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_meaning__originalist_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(us_c_su_t1989, us_constitution_meaning__originalist_reading, suppression_requirement, 1989, 0.48).
narrative_ontology:measurement(us_c_su_t1998, us_constitution_meaning__originalist_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(us_c_su_t2007, us_constitution_meaning__originalist_reading, suppression_requirement, 2007, 0.61).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_meaning__originalist_reading, suppression_requirement, 2016, 0.67).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_meaning__originalist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'constitutional interpretation methodology' per the eps-invariance principle. Each reading (originalist, living_constitutionalist, positivist) is authored as a structurally distinct constraint with its own eps, beneficiary/victim structure, and classification, linked here via affects_constraints. The originalist reading's high suppression and tangled_rope classification should not be read as characterizing the sibling readings, which are authored separately and may classify differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
