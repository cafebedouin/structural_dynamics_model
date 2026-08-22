% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federal Coercion Forcing Abandonment of Divine Requirement
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   plural_marriage_mandate kernel: the claim that the 1890 Manifesto ending
 *   institutional sanction of new plural marriages in the LDS Church
 *   represents coercive federal extraction of compliance, not legitimate
 *   doctrinal development. Under this reading, the sequence of escalating
 *   federal legislation (Morrill Act 1862, Edmunds Act 1882, Edmunds-Tucker
 *   Act 1887) culminating in church disincorporation and mass property
 *   seizure constitutes the actual causal mechanism producing the Manifesto;
 *   the revelatory framing subsequently attached to the declaration is, on
 *   this reading's own terms, a legitimating narrative laid over a coerced
 *   capitulation. Two sibling constraints instantiate different readings of
 *   the same kernel: the endogenous_reinterpretation_reading holds the
 *   Manifesto reflects genuine prophetic revelation: temporal divine
 *   suspension of a doctrine to preserve the church's salvific mission; the
 *   institutional_pragmatism_reading holds that doctrinal revelation claims
 *   function instrumentally to legitimate a survival-driven capitulation
 *   without asserting the coercion itself was illegitimate or that a divine
 *   mandate was actually overridden. This reading differs from both by
 *   asserting the pre-Manifesto commitment was a live divine requirement
 *   improperly overridden by external force, producing a genuine victim class
 *   (practicing polygamist families) whose losses this reading does not treat
 *   as offset by any legitimate revelatory resolution.
 *
 * KEY AGENTS:
 *   - federal_government: primary agenda-setter and beneficiary (institutional/analytical) — engineers territorial conformity through escalating coercive legislation
 *   - practicing_polygamist_families: primary victims (powerless/trapped) — bear imprisonment, property loss, and forced abandonment of a covenant this reading treats as divinely mandated
 *   - plural_wives_and_children: secondary and most powerless victims — bear derivative costs with no voice in either federal or church deliberation
 *   - church_hierarchy_under_duress: dual-positioned agent — administers the Manifesto's issuance while itself paying in institutional integrity and internal credibility under existential threat
 *   - utah_territorial_conformists: secondary beneficiaries — gain restored civic and economic normalcy without bearing the coercive costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.81).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Federal Coercion Forcing Abandonment of Divine Requirement").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '2e17b902-4785-4a74-924c-f129433718d2').
narrative_ontology:cs_kernel_codification('2e17b902-4785-4a74-924c-f129433718d2', formalized).
narrative_ontology:cs_authority_grounding('2e17b902-4785-4a74-924c-f129433718d2', lineage).
narrative_ontology:cs_interpretation_layer_present('2e17b902-4785-4a74-924c-f129433718d2').
narrative_ontology:cs_reading_relation('2e17b902-4785-4a74-924c-f129433718d2', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('2e17b902-4785-4a74-924c-f129433718d2', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('2e17b902-4785-4a74-924c-f129433718d2', foundational, plural_marriage_was_binding_divine_mandate_prior_to_1890).
narrative_ontology:cs_axiom_status(plural_marriage_was_binding_divine_mandate_prior_to_1890, holdable).
narrative_ontology:cs_axiom_grounding('2e17b902-4785-4a74-924c-f129433718d2', plural_marriage_was_binding_divine_mandate_prior_to_1890, deontological).
narrative_ontology:cs_axiom('2e17b902-4785-4a74-924c-f129433718d2', foundational, external_coercive_force_cannot_constitute_legitimate_doctrinal_revision).
narrative_ontology:cs_axiom_status(external_coercive_force_cannot_constitute_legitimate_doctrinal_revision, holdable).
narrative_ontology:cs_axiom_grounding('2e17b902-4785-4a74-924c-f129433718d2', external_coercive_force_cannot_constitute_legitimate_doctrinal_revision, deontological).
narrative_ontology:cs_reference_frame('2e17b902-4785-4a74-924c-f129433718d2', continuous_unbroken_prophetic_mandate_authority).
narrative_ontology:cs_drift_state('2e17b902-4785-4a74-924c-f129433718d2', post_edmunds_tucker_prosecution_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2e17b902-4785-4a74-924c-f129433718d2', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, utah_territorial_conformists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_hierarchy_under_duress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and escalates anti-polygamy legislation (Morrill, Edmunds, Edmunds-Tucker Acts), disincorporates the church, seizes its property, disenfranchises practicing polygamists, and imprisons church leaders until the Manifesto is issued. Achieves territorial conformity to national marriage norms and statehood prerequisites are met on its terms.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, federal_government, beneficiary).

% Face imprisonment of husbands and fathers, seizure of family property under the Edmunds-Tucker Act, disenfranchisement, and forced underground existence ('cohabs' hiding from federal marshals). Exit means either abandoning a marriage regarded as a sacred covenant or exposing the family to prosecution; there is no clean departure from the coercive structure once married.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamist_families, payer,
    powerless, biographical, trapped, regional).

% Bear the practical costs of the crackdown without having authored the marriages or the political conflict: loss of legal standing for the marriage, loss of inheritance and property claims, loss of a parent to prison, and social stigma both from federal prosecution and from post-Manifesto church distancing. Their exit options are the narrowest of any seat in the story.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children, payer,
    powerless, biographical, trapped, regional).

% The church presidency and apostles face a choice between continued imprisonment and institutional dissolution (property escheatment threatened territory-wide) or issuing a public declaration ending the sanctioning of new plural marriages. From this reading's lights, the declaration is authored under existential coercive pressure, not from theological conviction that the practice's time had passed; the leadership pays in institutional integrity and internal credibility even as it avoids further material destruction.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_hierarchy_under_duress, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_hierarchy_under_duress, agenda_setter).

% Non-polygamist residents, monogamous church members, and territorial business and political interests who benefit from the end of federal prosecution pressure, the restoration of church property and civic participation, and the path toward statehood the Manifesto opens. They do not bear the coercive costs the practicing polygamists absorbed.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, utah_territorial_conformists, beneficiary,
    moderate, generational, mobile, regional).

% Execute the raids, prosecutions, and property seizures that constitute the coercive apparatus. Their operational activity is the suppression mechanism this reading identifies as the actual cause of the Manifesto, rather than any independent theological development.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_marshals_and_prosecutors, agenda_setter,
    institutional, immediate, analytical, regional).

% The sibling reading holding that the Manifesto reflects genuine prophetic revelation ending a divine mandate for temporal reasons — treated here as a non-agent doctrinal claim excluded from this reading's account of causation, since this reading holds coercion, not revelation, as the operative mechanism.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, endogenous_reinterpretation_reading, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__exogenous_override_reading, endogenous_reinterpretation_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination problem being solved for the polygamist families themselves; the only coordination achieved is the federal government's goal of bringing Utah Territory into uniform compliance with national marriage law as a precondition for statehood, and the church's goal of institutional survival under existential threat.
% TRANSFER_FUNCTION: Moves practical control over marriage practice, family structure, church property, and civic enfranchisement from the church and its plural-marriage-practicing members to federal authority; moves the material and reputational cost of the conflict onto the polygamist families while the institutional church retains its core organizational assets and legal standing going forward.
% ABSENT_VOICES: Plural wives and children had no formal voice in either the federal legislative process or the church's internal deliberations that produced the Manifesto; their lived costs (loss of legal marriage status, property, parental incarceration) are documented mainly in family and community records rather than in the official narrative constructed by either the federal government or the church hierarchy.
% DISAPPEARANCE_RATIONALE: If federal anti-polygamy enforcement (prosecutions, property seizure, disincorporation threat) had not existed, this reading holds that the church would not have issued the Manifesto when it did; the practice's cessation timeline, the property disposition of the church, and the trajectory to statehood would all have proceeded on a materially different schedule or not at all in that form.
% FOUNDING_PROBLEM: From this reading's lights, the 'problem' the Manifesto solved was not a theological one but a coercive one: federal legislation and prosecution had made continued institutional operation of the church, and continued practice of plural marriage by its members, materially untenable through imprisonment and asset seizure.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary federal officials and territorial newspapers outside the church's own institutional voice documented the prosecutions, seizures, and disincorporation proceedings that preceded the Manifesto's issuance, and historians working from federal court and property records (independent of church archives) corroborate the timeline showing the Manifesto followed escalating coercive pressure rather than an independently dated revelatory claim; the church's own subsequent narrative (attesting ongoing revelatory guidance) is the account this reading treats as insufficient corroboration on its own.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at 1890) because, on this reading, the Manifesto represents the successful conversion of coercive pressure into a formally voluntary-appearing declaration — the extraction is masked, which is precisely what elevates theater_ratio over the interval (0.20 to 0.70) as the revelatory narrative is constructed and reinforced institutionally after the coercive episode. Suppression peaks sharply at 1887 (0.90) coinciding with the Edmunds-Tucker Act's disincorporation and property seizure provisions, the maximal coercive instrument in the sequence, before declining post-Manifesto as active federal prosecution wound down (0.35 by 1904) even as theater (the legitimating narrative) continued rising — the two curves diverge because ongoing narrative-construction work in the church's institutional memory does not require ongoing federal enforcement once compliance is achieved.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat, the outcome is a straightforward victory: legal compliance with national norms secured through legitimate exercise of federal authority over territorial governance. From the practicing polygamist families' seat, an active religious covenant was ended by external force under threat of family destruction. From the church hierarchy's seat under this reading, there is an uncomfortable double position — administering the very declaration that ends a practice its leadership had defended as divinely commanded, while simultaneously initiating the historical narrative that will reframe the coercion as revelation. The engine should compute a sharply different type from the payer seats (snare: coercive extraction, victims trapped) versus the agenda-setter seat (which experiences the same structure as legitimate law enforcement, closer to rope or even mountain-adjacent inevitability from the federal government's own account).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and federal marshals/prosecutors sit at the low-d beneficiary/agenda-setter end: they set the terms, escalate the pressure, and collect the outcome (territorial conformity, statehood pathway) without bearing the costs. Practicing polygamist families and especially plural wives and children sit at the high-d target end: trapped exit options, direct material and legal losses, and no voice in either legislative or ecclesiastical process. Church hierarchy occupies an intermediate position — it administers the resolution (agenda_setter secondary role) but under this reading pays real costs in institutional and personal integrity, justifying its dual role rather than placing it cleanly at either extreme. Utah territorial conformists are genuine indirect beneficiaries but with much lower stakes than the federal government, warranting moderate power rather than institutional.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists conflating the coordination the federal government achieved (territorial legal uniformity, a precondition treated as valuable for statehood) with the extraction it imposed on practicing polygamist families. Classifying the arrangement as snare rather than mountain or rope prevents the federal coercion from being naturalized as inevitable legal development; classifying it as snare rather than scaffold (which would require an honest sunset clause voluntarily adopted by the coerced party) prevents the coercive character of the transition from being obscured by the fact that a declaration was eventually issued. The mandatrophy risk this reading specifically guards against is treating the post-hoc revelatory narrative as dispositive evidence that the underlying process was legitimate doctrinal development rather than coerced capitulation — the theater_ratio trajectory exists to make that narrative-construction visible as a distinct, later-arising phenomenon from the coercive episode itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_causal_priority,
    'Was the timing and content of the 1890 Manifesto causally determined by federal legislative and prosecutorial pressure, or by an independent revelatory process that happened to align with that pressure?',
    'Comparative analysis of church leadership''s private correspondence and diary records from the period against the legislative and prosecutorial timeline; assessment of whether the specific content and timing of the declaration tracks federal legal deadlines (e.g., pending Supreme Court property forfeiture rulings) more closely than any independent doctrinal or liturgical marker.',
    'If the causal analysis strongly supports coercive timing (declaration issued in close proximation to concrete existential legal threats with no independent revelatory marker), this reading''s snare classification is strengthened. If independent revelatory markers predate or are decoupled from the legal pressure, the endogenous_reinterpretation_reading gains support and this reading''s causal claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_causal_priority, conceptual, 'Whether coercion or revelation is the operative causal mechanism behind the Manifesto''s timing and content.').

omega_variable(
    divine_mandate_ontological_status,
    'Was the pre-1890 practice of plural marriage genuinely divinely mandated in a sense that makes its cessation a real loss (as this reading presupposes), or was its doctrinal status itself contested or provisional within the tradition prior to 1890?',
    'Examination of internal church doctrinal statements and leadership disagreements about the permanence and universality of the plural marriage requirement prior to the federal crackdown, independent of post-Manifesto retrospective narrative.',
    'If the pre-1890 mandate was itself understood internally as contingent or already contested, the victim framing in this reading (a genuine divine requirement forcibly abandoned) is weakened, moving the analysis toward the institutional_pragmatism_reading''s bracketed position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_ontological_status, preference, 'Whether the antecedent divine mandate had a stable, uncontested doctrinal status this reading''s victim framing depends on.').

omega_variable(
    cs_framing_kernel_vs_authority_claim,
    'Should the contested kernel be framed as the specific 1890 declaration text, or as the broader legitimacy claim the church''s institutional authority structure depends on (continuous unbroken prophetic authority)?',
    'Compare classification outcomes under a narrower framing (kernel = the Manifesto text and its immediate causal history) versus a broader framing (kernel = the doctrine of continuous revelation as the legitimating basis for church authority, of which the Manifesto is one instance).',
    'Under the narrower framing (adopted here), this reading isolates the 1890 episode with a clean victim set and ε. Under the broader framing, the same causal claim would implicate every subsequent claimed revelation as similarly vulnerable to a coercion-vs-revelation ambiguity, which would change the classification from an event-specific snare to a standing structural question about the authority_grounding of the entire commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_authority_claim, conceptual, 'Whether the kernel is properly scoped to the 1890 Manifesto specifically or to the broader continuous-revelation authority claim it instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.2).
narrative_ontology:measurement(plur_tr_t1874, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1874, 0.28).
narrative_ontology:measurement(plur_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.35).
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.45).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.62).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.68).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.7).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(plur_be_t1874, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1874, 0.48).
narrative_ontology:measurement(plur_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.62).
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.78).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.81).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.66).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(plur_su_t1874, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(plur_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.68).
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.9).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.88).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.55).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the plural_marriage_mandate kernel, decomposed per the ε-invariance principle because the three readings assign structurally different extractiveness, victim sets, and types to the same underlying historical episode depending on the causal and normative framework applied. exogenous_override_reading (this story) authors high ε (0.81) and type snare, with practicing polygamist families as victims and the federal government as beneficiary. endogenous_reinterpretation_reading is expected to author low ε and type rope or mountain-adjacent (genuine doctrinal development, no coerced victim class). institutional_pragmatism_reading is expected to author moderate ε and type tangled_rope (real institutional survival coordination function, but with the coercion's asymmetric cost still present, bracketing the divine-mandate question). All three link to each other via affects_constraints to preserve the kernel's contest structure for network-level contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
