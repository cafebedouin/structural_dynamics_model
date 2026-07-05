% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy: Courts as Final Constitutional Interpreters
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the judicial supremacy reading of the
 *   constitutional interpretive authority kernel: courts hold final say over
 *   what the constitution means, and legislative acts inconsistent with that
 *   reading are void, not merely disfavored. This is a genuine coordination
 *   structure — it solves the real problem of entrenching rights against
 *   majoritarian reversal — but it also runs an asymmetric extraction:
 *   unelected judiciary gains durable, unaccountable authority over policy
 *   terrain that in a legislative-supremacy or coordinate-construction
 *   reading would remain contestable through ordinary politics. The rising
 *   extractiveness trajectory reflects doctrinal expansion over the interval
 *   (courts progressively widening the scope of 'rights' subject to review,
 *   from narrow enumerated guarantees toward broader implied doctrines)
 *   without a corresponding expansion of democratic accountability
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda_setter/beneficiary (institutional/arbitrage) — sets and applies the doctrine that entrenches its own final-word status
 *   - electoral_majorities: payer (organized/constrained) — bears the cost when legislation reflecting majority will is nullified
 *   - legislature: payer/excluded (organized/constrained) — retains formal lawmaking power but not final authority over outcomes
 *   - rights_bearing_minorities: beneficiary (powerless/trapped) — depends on judicial nullification as primary structural protection
 *   - constitutional_lawyers: beneficiary (organized/mobile) — professional capital tied to the arrangement's persistence
 *   - executive_branch: payer (institutional/constrained) — implements policy subject to judicial veto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy: Courts as Final Constitutional Interpreters").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '5c672f2a-627d-4b0c-aca2-df0982d2281a').
narrative_ontology:cs_kernel_codification('5c672f2a-627d-4b0c-aca2-df0982d2281a', formalized).
narrative_ontology:cs_authority_grounding('5c672f2a-627d-4b0c-aca2-df0982d2281a', lineage).
narrative_ontology:cs_interpretation_layer_present('5c672f2a-627d-4b0c-aca2-df0982d2281a').
narrative_ontology:cs_reading_relation('5c672f2a-627d-4b0c-aca2-df0982d2281a', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('5c672f2a-627d-4b0c-aca2-df0982d2281a', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('5c672f2a-627d-4b0c-aca2-df0982d2281a', foundational, judicial_finality_over_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_over_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('5c672f2a-627d-4b0c-aca2-df0982d2281a', judicial_finality_over_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('5c672f2a-627d-4b0c-aca2-df0982d2281a', secondary, rights_entrenchment_requires_countermajoritarian_enforcement).
narrative_ontology:cs_axiom_status(rights_entrenchment_requires_countermajoritarian_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('5c672f2a-627d-4b0c-aca2-df0982d2281a', rights_entrenchment_requires_countermajoritarian_enforcement, instrumental).
narrative_ontology:cs_reference_frame('5c672f2a-627d-4b0c-aca2-df0982d2281a', textually_bounded_judicial_review).
narrative_ontology:cs_drift_state('5c672f2a-627d-4b0c-aca2-df0982d2281a', contemporary_rights_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c672f2a-627d-4b0c-aca2-df0982d2281a', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearing_minorities).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_lawyers).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_rights_protection_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the power to declare legislative and executive acts void for incompatibility with constitutional text or fundamental rights. Sets the doctrinal tests (strict scrutiny, proportionality, basic structure) by which its own authority is measured, and cannot be overruled by ordinary legislative majority. Collects institutional prestige, docket control, and freedom from electoral accountability as the price of being the last word.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Elect legislators who pass statutes reflecting majority preference, only to see those statutes nullified by unelected judges applying a constitutional text the majority did not choose the wording of and cannot easily amend. Their remedy — constitutional amendment or court-packing — carries supermajority thresholds or high political cost, so in practice the ruling stands regardless of subsequent electoral outcomes.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Drafts and passes statutes that can be struck down after the fact by a body it does not control and cannot instruct. Retains formal power to legislate but not final power to make legislation stick; every statute touching rights-adjacent terrain is drafted anticipating judicial review, which shapes legislative drafting toward what courts will tolerate rather than what constituents demanded.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, excluded).

% Groups without durable electoral majorities depend on judicial nullification of majoritarian legislation to protect rights that a legislative majority would otherwise vote away. Cannot secure protection through ordinary electoral politics because they lack the votes; the judiciary's countermajoritarian power is their primary structural defense, though it depends entirely on the composition and disposition of a court they did not elect and cannot remove.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearing_minorities, beneficiary,
    powerless, generational, trapped, national).

% A specialized bar and academy exists to litigate before, clerk for, theorize about, and credential entry into the constitutional review process. Their professional standing, billing rates, and academic output are all downstream of judicial supremacy being the operative arrangement; a shift to legislative or coordinate-construction models would substantially devalue this specialized capital.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_lawyers, beneficiary,
    organized, biographical, mobile, national).

% Implements and defends policy that can be invalidated by judicial order, including emergency and security measures. Must litigate to defend executive action and comply with adverse rulings even where the executive represents a fresh electoral mandate; noncompliance risks a constitutional crisis the executive typically cannot win in institutional terms.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% The founding document and its historical drafters are invoked by all sides as the source of legitimate authority, but as a non-agent text it cannot itself adjudicate between competing readings of who gets final say over what it means.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_text_and_drafters_intent, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_text_and_drafters_intent).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, depoliticized forum for resolving disputes about the boundaries of governmental power and the content of fundamental rights, insulated from shifting electoral majorities, so that basic entitlements are not subject to majority-vote reversal each election cycle.
% TRANSFER_FUNCTION: Moves final say over the meaning and application of constitutional guarantees from elected legislatures to appointed or life-tenured judges; moves protection for minority and rights claims away from dependence on legislative goodwill and toward dependence on judicial composition and doctrine.
% ABSENT_VOICES: Electoral majorities whose statutes are struck down have no formal channel to contest the ruling except constitutional amendment (typically supermajority-gated) or altering court composition through appointments over years — both far slower and higher-friction than the ordinary legislative process that produced the nullified statute. Future electorates whose preferences differ from the enacting generation are also structurally absent from the interpretive act.
% DISAPPEARANCE_RATIONALE: If judicial nullification power vanished overnight, legislatures would immediately regain unchallenged authority to enact and repeal rights-touching statutes, minority protections would depend entirely on ongoing legislative coalitions rather than entrenched doctrine, the constitutional bar's specialized litigation function would collapse into ordinary statutory practice, and executive action would no longer face judicial veto — a wholesale reallocation of governmental power, not a cosmetic change.
% FOUNDING_PROBLEM: Written constitutions needed an enforcement mechanism: without some body empowered to say a statute violates fundamental commitments and to make that violation consequential, constitutional guarantees risked being merely aspirational, revocable by whatever legislative majority happened to hold power.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and constitutional scholars attest the enforcement problem remains live — majorities do periodically attempt rights-restricting legislation. Legislators and political theorists outside the judiciary and legal academy (e.g., parliamentary-supremacy scholars, democratic theorists) attest that the arrangement has drifted from enforcement-of-settled-text toward substantive policymaking by an unaccountable body, and that legislatures could enforce constitutional commitments through their own oath-bound review absent judicial nullification. No source entirely outside the legal profession corroborates the strong version of the founding claim; this is itself notable.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than high because the coordination function — protecting entrenched rights from majoritarian reversal — is genuine and substantial, not mere cover; the extraction lies specifically in the asymmetry between who bears the cost of nullification (legislatures, electoral majorities, executive) and who captures durable authority from it (judiciary, and secondarily the specialized bar). Suppression (0.55) reflects that noncompliance with judicial rulings is not optional for legislatures or executives — enforcement runs through contempt power, injunctive machinery, and the delegitimizing force of 'unconstitutional' as a label, though it stops short of the suppression profile of a pure extraction mechanism because political actors retain the amendment and appointment channels, however costly. Theater ratio is low-to-moderate and rising (0.12 to 0.28) — most judicial review activity is substantively consequential rather than performative, but the rising trend reflects an increasing share of high-profile rulings functioning more as institutional assertions of authority than close textual adjudication.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is a coordination mechanism it maintains faithfully in service of constitutional fidelity. From the legislature's and electoral majority's seat, the same structure computes as an enforced subordination: their democratically-produced output is contingent on a body they cannot instruct or remove. The engine should register this as seat divergence rather than resolve it — the judicial seat's account and the legislative seat's account are both structurally coherent readings of the same arrangement from different positions of power and exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the clearest beneficiary: it acquires durable interpretive authority, institutional insulation from electoral swings, and no formal mechanism subordinates it back to the branches it reviews (arbitrage-level exit from accountability). Rights-bearing minorities are also structural beneficiaries in outcome even though they hold no power over the mechanism itself — their d sits low because the constraint's core function (protecting them from majoritarian reversal) directly serves them, notwithstanding their powerlessness elsewhere. Electoral majorities, the legislature, and the executive are targets: their preferences and enacted policy are subject to override by a non-accountable body, and their formal remedies (amendment, appointment turnover) are high-friction relative to the speed of the extraction. Constitutional lawyers are secondary beneficiaries via professional capital rather than through the rights-protection function itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — entrenching rights against majoritarian reversal — remains partially live (minorities without durable majorities continue to need protection from majority-vote policy), which is why this is authored tangled_rope rather than snare: there is a real coordination function still operating, not merely inertial extraction. But the founding_problem_status is authored contested because substantial evidence (rising extractiveness, doctrinal scope expansion, theater ratio increase) suggests the arrangement has partly drifted from enforcing settled textual commitments toward the judiciary exercising open-ended policymaking discretion under the enforcement label. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating it as pure extraction (which would erase the genuine minority-protection function) and treating it as natural/inevitable judicial authority (which would erase the beneficiary structure and the contestability the other kernel readings assert).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_review_scope_creep,
    'Has judicial nullification power drifted from enforcing the original enumerated constitutional text toward open-ended substantive policymaking under a rights-protection label, and if so, at what point did enforcement become extraction?',
    'Longitudinal doctrinal analysis comparing the scope of constitutional questions courts entertained at founding versus at present; comparison of vote margins and dissent patterns on cases expanding versus applying existing doctrine.',
    'If scope creep is substantial and undisclosed, the tangled_rope classification understates extraction and the constraint drifts toward snare; if scope has remained stable and expansion tracks genuine textual and precedential development, the coordination function is more robust than the rising extractiveness measurements suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_scope_creep, empirical, 'Whether judicial review has expanded beyond its founding scope into policymaking.').

omega_variable(
    counter_majoritarian_legitimacy_puzzle,
    'Is unelected judicial override of majoritarian legislation a legitimate check on tyranny of the majority, or an unaccountable transfer of governing power to an unelected elite dressed in rights language?',
    'This is not resolvable by data alone — it depends on contested normative commitments about the relationship between democracy and rights protection. Comparative analysis across jurisdictions with different models (judicial supremacy vs. parliamentary supremacy vs. coordinate construction) can surface empirical correlates (rights outcomes, policy stability) but cannot settle the underlying value question.',
    'Resolution toward ''legitimate check'' supports treating the beneficiary structure as justified counter-majoritarianism (rope-adjacent); resolution toward ''unaccountable transfer'' supports treating it as institutionalized extraction (snare-adjacent). The tangled_rope classification is the deliberate middle position pending this unresolved question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_legitimacy_puzzle, preference, 'Whether counter-majoritarian judicial review is legitimate rights protection or unaccountable power transfer — a values question, not an empirical one.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading (judicial supremacy) of the constitutional_interpretive_authority kernel. The sibling readings (parliamentary supremacy, coordinate construction) assign final authority to entirely different seats. Which reading a given constitutional order actually instantiates is itself contested within and across jurisdictions and over time within the same jurisdiction.',
    'Comparative constitutional analysis tracking which branch''s rulings/acts prevail when in direct conflict, across a sample of jurisdictions and historical periods; also track formal doctrine (does the constitution explicitly grant nullification power, or is it judicially self-asserted as in Marbury-style systems).',
    'A jurisdiction that structurally instantiates the parliamentary supremacy or coordinate construction reading instead would have an entirely different beneficiary/victim structure and a markedly different ε for this same nominal domain — that is why this constraint is authored separately from its siblings rather than as one story with a variable interpretive-authority parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'This story instantiates one of three structurally distinct readings of who holds final constitutional interpretive authority; sibling readings are separate constraints, not alternative measurements of this one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t14, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 14, 0.16).
narrative_ontology:measurement(cons_tr_t28, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 28, 0.19).
narrative_ontology:measurement(cons_tr_t42, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 42, 0.22).
narrative_ontology:measurement(cons_tr_t56, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 56, 0.25).
narrative_ontology:measurement(cons_tr_t70, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cons_be_t14, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 14, 0.28).
narrative_ontology:measurement(cons_be_t28, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 28, 0.33).
narrative_ontology:measurement(cons_be_t42, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 42, 0.37).
narrative_ontology:measurement(cons_be_t56, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 56, 0.4).
narrative_ontology:measurement(cons_be_t70, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t14, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 14, 0.45).
narrative_ontology:measurement(cons_su_t28, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement(cons_su_t42, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 42, 0.51).
narrative_ontology:measurement(cons_su_t56, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 56, 0.53).
narrative_ontology:measurement(cons_su_t70, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings sharing the constitutional_interpretive_authority kernel. judicial_supremacy_reading (this story): judiciary is the beneficiary of final interpretive authority, legislature and electoral majorities are payers, ε=0.42, tangled_rope. parliamentary_supremacy_reading: legislature holds final authority, judiciary has no nullification power, the beneficiary/victim structure inverts. coordinate_construction_reading: no single branch holds final authority; meaning is settled through ongoing inter-branch contestation, producing a more diffuse, lower-ε structure without a dominant beneficiary. All three are linked via affects_constraints; none should be treated as a measurement of the same underlying constraint under a different observable — they are structurally distinct arrangements with different ε, different stakeholders, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
