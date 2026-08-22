% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Meaning
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the living constitutionalist reading of the
 *   contested us_constitution_meaning kernel: constitutional text encodes
 *   enduring principles (liberty, equal protection, due process) whose
 *   concrete application is understood to legitimately track evolving social
 *   attitudes, scientific knowledge, and changed circumstances, as elaborated
 *   by the judiciary. This is a distinct constraint from the
 *   originalist_reading (meaning fixed at ratification) and the
 *   positivist_reading (validity from formal enactment procedure alone) — the
 *   three readings are not measured on one scale; each is authored as its own
 *   constraint with its own epsilon, beneficiary/victim structure, and
 *   classification, linked through network.affects_constraints. The referent
 *   for extractiveness here is the standing living-constitutionalist
 *   arrangement as currently practiced by federal courts, assessed by this
 *   reading's own lights — not the reading's aspirational ideal of principled
 *   elaboration.
 *
 * KEY AGENTS:
 *   - rights_claimants_in_evolving_social_contexts: primary beneficiary (moderate/constrained) — obtains recognition unavailable under fixed historical meaning
 *   - sitting_judiciary: agenda_setter (institutional/arbitrage) — administers the interpretive method and decides what counts as legitimate elaboration
 *   - legislative_majorities: primary payer (organized/constrained) — enactments invalidated by judicially updated constitutional understanding
 *   - losing_litigants_under_reinterpretation: payer (moderate/trapped) — bear reliance costs of doctrinal instability
 *   - originalist_judiciary_and_scholars: excluded rival interpretive community (organized/constrained)
 *   - constitutional_law_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.42).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '2783ce48-60b7-42af-b080-15a570854e87').
narrative_ontology:cs_kernel_codification('2783ce48-60b7-42af-b080-15a570854e87', fixed_text).
narrative_ontology:cs_authority_grounding('2783ce48-60b7-42af-b080-15a570854e87', lineage).
narrative_ontology:cs_interpretation_layer_present('2783ce48-60b7-42af-b080-15a570854e87').
narrative_ontology:cs_reading_relation('2783ce48-60b7-42af-b080-15a570854e87', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2783ce48-60b7-42af-b080-15a570854e87', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('2783ce48-60b7-42af-b080-15a570854e87', foundational, constitutional_principles_admit_evolving_application).
narrative_ontology:cs_axiom_status(constitutional_principles_admit_evolving_application, holdable).
narrative_ontology:cs_axiom_grounding('2783ce48-60b7-42af-b080-15a570854e87', constitutional_principles_admit_evolving_application, conventional).
narrative_ontology:cs_axiom('2783ce48-60b7-42af-b080-15a570854e87', secondary, contemporary_moral_consensus_is_interpretively_relevant).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_is_interpretively_relevant, holdable).
narrative_ontology:cs_axiom_grounding('2783ce48-60b7-42af-b080-15a570854e87', contemporary_moral_consensus_is_interpretively_relevant, instrumental).
narrative_ontology:cs_reference_frame('2783ce48-60b7-42af-b080-15a570854e87', brown_era_evolving_application_consensus).
narrative_ontology:cs_drift_state('2783ce48-60b7-42af-b080-15a570854e87', post_2010s_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2783ce48-60b7-42af-b080-15a570854e87', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_social_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, sitting_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, losing_litigants_under_reinterpretation).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, evolving_standards_of_decency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose claims were not contemplated or were affirmatively rejected at ratification (e.g., claims to marriage equality, expanded due process protections, incorporation of rights against the states) obtain judicial recognition of those claims by appeal to enduring constitutional principle applied to present circumstances rather than to fixed historical practice at the founding. Their access to relief depends entirely on courts accepting that application evolves; without that premise their claims have no textual or historical purchase.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_social_contexts, beneficiary,
    moderate, generational, constrained, national).

% Federal judges, especially at the appellate and Supreme Court level, administer this reading by deciding which contemporary moral and social developments count as legitimate elaborations of enduring principle versus illegitimate judicial invention. They set the interpretive method itself, are largely insulated from majoritarian correction (life tenure, supermajority amendment threshold), and their discretion is the mechanism through which the reading operates.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, sitting_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Elected majorities that enact statutes reflecting current popular preference find those statutes invalidated when courts determine the enactment conflicts with a judicially updated understanding of enduring principle. Their recourse is constitutional amendment (practically foreclosed by supermajority requirements) or waiting out judicial composition change through the appointments process — both slow, uncertain, and largely outside ordinary legislative control.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).

% Parties who relied on settled precedent or the historical understanding of a provision find their legal position reversed when a later court determines that contemporary values require a different application of the same enduring text. They bear the cost of doctrinal instability with no advance notice and no vote on the change; the same interpretive method that helped one claimant a decade earlier can defeat a reliance interest the next.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, losing_litigants_under_reinterpretation, payer,
    moderate, biographical, trapped, national).

% Judges and scholars committed to fixed original public meaning argue that this reading substitutes judicial moral judgment for democratically enacted or ratified text, and that it lacks a principled stopping point. They participate in the same judicial system and academic discourse but their competing interpretive method is treated as one contestable school among several rather than as dispositive; they cannot unilaterally displace the living reading where it commands a judicial majority.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_judiciary_and_scholars, excluded,
    organized, generational, constrained, national).

% Legal academics study how the doctrine of evolving application operates across cases, track its consistency and its departures from precedent, and debate whether it functions as principled elaboration or as judicial policymaking dressed in constitutional language.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a 235-year-old founding text with fixed, sparse language to remain a workable governing document for circumstances the drafters could not have anticipated (electronic surveillance, corporate personhood questions, changed family structures, medical technology) without requiring constant formal amendment, which the Constitution's supermajority mechanism makes extremely difficult to obtain.
% TRANSFER_FUNCTION: Moves interpretive authority over contested social questions from legislatures and the amendment process toward the judiciary, and moves substantive legal outcomes from what majorities currently enact toward what a judicial majority determines an enduring principle now requires — benefiting claimants whose position could not prevail through ordinary lawmaking and imposing costs on majorities and reliance-interest holders whose expectations are unsettled.
% ABSENT_VOICES: Losing litigants under later reinterpretation are rarely represented in the case that reinterprets the doctrine against them — the party who benefits from the new reading is before the court; the party whose settled expectations under the old reading are disrupted is often a future litigant not yet before any court. Legislative majorities whose enactments are invalidated have no formal voice within the judicial proceeding itself, only through amicus participation.
% DISAPPEARANCE_RATIONALE: If courts abandoned the premise that application evolves and adopted strict fixed-meaning interpretation across the board, a substantial body of existing constitutional doctrine — incorporation of the Bill of Rights against the states, modern due process and equal protection jurisprudence, privacy-based rulings — would lose its interpretive foundation and become vulnerable to reversal; legislatures would regain primary authority over many currently constitutionalized questions.
% FOUNDING_PROBLEM: The Constitution's text is deliberately general in many clauses (due process, equal protection, cruel and unusual punishment) and the amendment process is designed to be difficult, creating a structural need for some mechanism by which the document's application can track changed social understanding without formal textual amendment for every new circumstance.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalist judges and scholars attest the problem is live and the doctrine solves it faithfully. Originalist scholars and jurists — a constituency outside those who benefit from expanded rights recognition — attest that the 'problem' as framed is itself a justification invented to license judicial policymaking, and that the amendment process, however difficult, is the constitutionally designated mechanism for updating meaning; they do not corroborate the founding-problem narrative as stated.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising modestly: the reading has a genuine coordination function (keeping a hard-to-amend text workable across changed circumstance) but the same discretion that enables rights expansion also enables outcomes untethered from any electorally accountable process, which is where the extraction from legislative majorities and reliance-interest holders occurs. Suppression is moderate-low (0.38) relative to the originalist reading's structural profile, because this reading by design keeps more avenues open — precedent can move again, the doctrine does not foreclose future reconsideration the way a fixed-meaning rule would. Theater ratio (0.28) reflects that a meaningful share of 'evolving application' reasoning is genuine doctrinal work, though some opinions invoke 'evolving standards' as post-hoc justification for outcomes reached on other grounds. Resistance is comparatively high (0.62) because the reading is persistently and vocally contested by an organized rival interpretive tradition (originalism) with real institutional power (appointments, scholarship, some judicial majorities) — this is not a settled, uncontested arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the sitting judiciary's seat, this operates as principled, disciplined elaboration of enduring commitments — a genuine coordination function solving the fixed-text/changing-world problem. From the legislative-majority seat, the same practice looks like extraction of policymaking authority into an unaccountable branch: laws passed through the constitutionally designated process are set aside by reference to a moral consensus no one voted for. The engine computing divergent per-seat classifications from this same structural data is the point — I am not resolving which seat is 'right,' only authoring the structure honestly from each position.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants in evolving social contexts derive low d: the reading's operation subsidizes their legal position by providing an avenue unavailable under the sibling readings. The sitting judiciary, as agenda_setter with life tenure and effectively final interpretive authority (arbitrage-grade exit from ordinary accountability), also sits toward the low-d end structurally, though it does not collect a material rent — its 'benefit' is interpretive discretion itself. Legislative majorities and losing litigants under reinterpretation derive high d: their enactments or settled reliance positions are the object the doctrine unsettles, and their exit options (amendment, waiting for judicial turnover) are slow and largely outside their control, which the engine should read as constrained rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a sparse, hard-to-amend text needing some mechanism to track changed circumstance — remains genuinely live for large classes of constitutional questions (privacy against new technology, due process in new institutional settings), which argues against a clean piton or snare reading. But the tangled_rope classification captures the honest structure: real coordination (text stays usable) coexists with real, identifiable extraction (legislative and reliance interests overridden without their own consent channel), and the arrangement requires active judicial enforcement to persist against a well-organized rival reading. Calling this a pure rope would erase the extraction on legislative majorities; calling it a pure snare would erase the genuine coordination benefit for rights claimants who have no other avenue. Tangled rope is the type that holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    principled_elaboration_vs_policy_substitution,
    'Is judicial determination of ''evolving standards'' a disciplined application of enduring constitutional principle, or a mechanism by which judges substitute their own policy preferences for text and history under cover of principle?',
    'Track consistency: does the doctrine constrain outcomes the deciding judges personally favor as often as it enables outcomes they favor? A doctrine that only ever expands in the direction of the judiciary''s own preferences, never against them, is evidence for the policy-substitution reading.',
    'If principled elaboration, the tangled_rope classification''s coordination component is well-supported and extraction is closer to incidental. If policy substitution, the coordination story is largely cover and the constraint is closer to a snare on legislative majorities and reliance-interest holders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(principled_elaboration_vs_policy_substitution, conceptual, 'Whether evolving-application doctrine constrains judges or merely licenses their preferences.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the living constitutionalist reading correctly modeled as a rival interpretive METHOD competing with originalism and positivism, or as a claim about the Constitution''s own self-understanding that the other readings deny?',
    'Examine whether founding-era evidence supports an original expectation that later interpreters would update application (e.g., general-clause drafting choices, Marshall-era interpretive practice) versus evidence that ratifiers expected fixed original meaning to govern.',
    'If the Constitution''s own drafters anticipated evolving application, this reading''s axioms shift from purely normative/interpretive claims toward historically grounded claims, which would strengthen its position relative to the originalist reading''s own foundational premise. If not, this reading rests more heavily on external moral/political theory than on constitutional self-understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether this reading''s premise is itself partly historically grounded or purely normative/interpretive.').

omega_variable(
    counter_majoritarian_cost_measurement,
    'How should the cost imposed on legislative majorities and reliance-interest holders be weighted against the benefit to previously unrecognized rights claimants — is this a genuine victim class or a normal, expected cost of constitutional judicial review under any reading?',
    'Compare invalidation rates and reversal-of-settled-expectation rates under this reading versus under the originalist and positivist readings across a matched set of case types.',
    'If invalidation/reversal rates are not meaningfully higher under this reading than under its siblings, the victim declaration here may overstate this reading''s distinctive extraction relative to judicial review generally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_cost_measurement, empirical, 'Whether the counter-majoritarian cost is distinctive to this reading or a general feature of judicial review.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(us_c_be_t80, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(us_c_be_t100, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(us_c_su_t80, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(us_c_su_t100, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the us_constitution_meaning kernel (living_constitutionalist, originalist, positivist). Each reading is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and classification per the epsilon-invariance principle — the kernel's contested meaning is not modeled as one constraint with an observable-dependent value, but as three constraints linked by network edges. The living constitutionalist reading is expected to show lower suppression of rights-expansion claims and higher counter-majoritarian extraction relative to the originalist reading; the positivist reading is expected to show a structurally different beneficiary/victim map centered on procedural regularity rather than either historical fixity or moral evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
