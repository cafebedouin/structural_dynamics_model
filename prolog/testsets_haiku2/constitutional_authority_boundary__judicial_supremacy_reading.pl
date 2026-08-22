% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the judicial-supremacy reading of the
 *   constitutional-authority-boundary kernel. The constitutional text is
 *   ambiguous on whether courts possess final interpretive authority over all
 *   constitutional questions, and different readings distribute that
 *   authority differently. This story models the reading that claims courts
 *   are the unchallengeable final arbiters, authorized to invalidate
 *   legislative and executive acts without remedy except through formal
 *   amendment. The structural consequence: the judiciary becomes a
 *   beneficiary (collecting interpretive monopoly rents), the legislature
 *   becomes a target (constrained policy space without recourse), and the
 *   constraint operates as tangled rope — genuine coordination function
 *   (stable constitutional meaning, individual-rights protection) yoked to
 *   asymmetric extraction (judicial veto power). The claim-to-metric
 *   divergence is deliberate: the judiciary's own legitimating narrative
 *   frames this as rule of law (rope), while the authored metrics describe
 *   substantially extractive, actively suppressed operation whose escalation
 *   tracks expansion of the judicial docket and doctrinal ambition.
 *
 * KEY AGENTS:
 *   - Judiciary: agenda-setter (institutional power, finalizes constitutional meaning; monopoly on lawful override)
 *   - Legislature: payer (institutional power, constrained by judicial veto; no institutional remedy except formal amendment)
 *   - Executive: payer (institutional power, operationally dependent on judicial cooperation; no override option)
 *   - Constitutional bar: beneficiary and co-agenda-setter (professional power, arbitrage-grade exit, rents from expanded docket and interpretive scope)
 *   - Ordinary citizens: dual-positioned beneficiary and indirect payer (powerless, trapped, benefit from rights protection and rule-of-law stability; carry cost of judicial veto of popular legislation)
 *   - Constitutional critics: excluded (moderate power, constrained exit, critique the reading but hold no institutional seat in adjudication; amendment is their only recourse)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional/institutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '4c4fb965-f3d0-4221-b30c-6987ac3b8c10').
narrative_ontology:cs_kernel_codification('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', formalized).
narrative_ontology:cs_authority_grounding('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', extraction).
narrative_ontology:cs_interpretation_layer_present('4c4fb965-f3d0-4221-b30c-6987ac3b8c10').
narrative_ontology:cs_reading_relation('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', foundational, judicial_review_inherent_to_article_iii).
narrative_ontology:cs_axiom_status(judicial_review_inherent_to_article_iii, holdable).
narrative_ontology:cs_axiom_grounding('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', judicial_review_inherent_to_article_iii, empirically_contingent).
narrative_ontology:cs_axiom('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', foundational, constitutional_meaning_requires_final_arbiter).
narrative_ontology:cs_axiom_status(constitutional_meaning_requires_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', constitutional_meaning_requires_final_arbiter, deontological).
narrative_ontology:cs_reference_frame('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', marbury_judicial_review_doctrine).
narrative_ontology:cs_drift_state('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', contemporary_rights_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4c4fb965-f3d0-4221-b30c-6987ac3b8c10', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text with final authority; invalidates legislative and executive acts deemed unconstitutional; controls constitutional meaning through doctrine; benefits from monopoly on interpretive authority; enforcement of this reading depends on legislatures accepting nullification and no branch successfully asserting coordinate authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Enacts legislation subject to judicial nullification; has no recourse except formal amendment (a supermajority bar); constrained by judicial interpretation of enumerated powers; cannot reassert interpretive authority without confronting entrenched doctrine; pays the cost of policy space denial and legal uncertainty.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, generational, constrained, national).

% Operates under judicial constraint; executive interpretation receives no deference; enforcement of statutes depends on judicial cooperation; appointment influence does not reverse the structural subordination to court's constitutional reading.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive, payer,
    institutional, biographical, constrained, national).

% Professionals who benefit from expanded constitutional litigation docket; expertise centers on persuading courts, not legislatures; professional identity fused with presumption that courts are proper arbiters; doctrinal development creates career paths and authority; can exit into other legal practice but at cost of identity repositioning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar, agenda_setter).

% Benefit from individual-rights protection and rules predictability; carry indirect cost where judicial veto blocks popular legislation; have no institutional seat in constitutional interpretation; cannot exit the constitutional order (binding by residence and citizenship).
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens, payer).

% Scholars and movement leaders holding coordinate-construction or parliamentary-primacy readings; excluded from judicial process itself; can contest only through constitutional amendment campaigns or political pressure; their preferred readings are not recognized as valid within the authoritative legal process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_critics, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a final, stable interpretive authority for constitutional meaning across generations, protects individual constitutional rights from majoritarian erosion through ordinary legislation, provides a credible check on executive overreach, maintains constitutional text as binding law rather than political instrument.
% TRANSFER_FUNCTION: Moves interpretive authority from coordinate branches and electoral majorities to the judiciary; moves constitutional litigation (and associated professional rents) into courts as the sole authoritative venue; moves policy-space determination from legislatures to judges; transfers the cost of constitutional nullification (and the inability to override it except through supermajority amendment) from courts to legislatures and executives.
% ABSENT_VOICES: Coordinate-construction and parliamentary-primacy readings are excluded from the authoritative legal process: scholars holding these views participate only as litigants or amici, not as authorized interpreters; legislatures themselves cannot assert co-equal interpretive authority within the existing framework without first challenging judicial supremacy doctrine; movements for constitutional reframing have no institutional seat except political amendment campaigns.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished and coordinate-construction or parliamentary readings became authoritative instead, the institutional balance would reorganize: legislatures would recover interpretive authority and policy space, executives would receive deference in constitutional questions within their sphere, constitutional amendments would become unnecessary for overriding unpopular judicial rulings, and the constitutional bar would lose its monopoly litigation market. The foundational institution would shift from courts as arbiter to distributed authority or legislative supremacy.
% FOUNDING_PROBLEM: The constitutional text is ambiguous on whether courts possess final interpretive authority; early republic contest over whether each branch interprets constitutionally within its sphere (coordinate construction) or whether courts have inherent power of judicial review (judicial supremacy); Marbury v. Madison (1803) resolved the ambiguity by claiming judicial review as inherent to the judicial role.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Randy Barnett, Jack Balkin) and constitutional law critics (Larry Kramer on popular constitutionalism, Keith Whittington on coordinate construction) argue that judicial supremacy is not the text's only defensible reading and that Marbury was a choice, not a logical necessity. Foreign democracies operating under coordinate-construction (Australia) or parliamentary-supremacy (Canada with notwithstanding clause, UK) readings demonstrate alternative readings are institutionally viable. The judiciary and bar attest that final adjudication is necessary for constitutional stability; the legislature and critics dispute both the necessity and the inevitability. No corroborating source outside the judicial beneficiary set endorses judicial supremacy as the founding problem's necessary solution.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because judicial interpretation determines constitutional scope without constraint from coordinate branches or democratic revision except through supermajority amendment — a 60+% margin for suppressing alternative readings. The constraint's persistence depends on active enforcement: legislatures must accept invalidation, executive actors must comply with judicial construction, and alternative readings (coordinate-construction, parliamentary-primacy) must be excluded from the authoritative legal process. Suppression is high (0.72) because coordination benefits alone (stable meaning, rights protection) would not sustain a constraint that denies legislatures and executives any institutional voice in constitutional interpretation; the suppression maintains the monopoly. Theater ratio (0.41) reflects that a large share of judicial constitutional work is legitimacy-maintenance: opinions written to justify why courts are the proper arbiters, historical narratives about Marbury and its legacy, doctrinal development that expands interpretive scope while maintaining the appearance of discovering rather than creating meaning. The measurement series track the constraint's intensification from 1803 (Marbury, t=0) through the 20th-century expansion of judicial docket and the modern amplification of judicial review scope; extraction rises as doctrinal categories proliferate and as political contestation drives more cases into court.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary's seat computes the constraint as rope (genuine coordination, minimal extraction) because its beneficiaries experience only the stable-interpretation benefit and none of the veto-cost. The legislature's seat computes it as snare (pure extraction, coordination is secondary) because its payers experience the policy nullification without any corresponding benefit and no institutional remedy. The constraint's operation hinges on this divergence: the judiciary frames the rule in legitimacy terms (rule of law, constitutional stability); the legislature experiences the rule as suppression (policy space denied, coordination function could be achieved without veto power). The engine computes per-seat, and that computation captures the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary (institutional, analytical exit): d ≈ 0.05 — full beneficiary, collects interpretive authority and rents from monopoly litigation market, can exit to different constitutional readings only through repudiation (analytical exit). Legislature and Executive (institutional, constrained exit): d ≈ 0.95 — full targets, bear the cost of policy nullification without remedy short of supermajority amendment, cannot exit except through extra-constitutional action. Constitutional bar (powerful, arbitrage exit): d ≈ 0.15 — beneficiary, professional rents ride the expansive docket, can exit into other legal practice but maintains professional identity fusion with judicial supremacy. Ordinary citizens (powerless, trapped): d ≈ 0.50 — symmetric, benefit from coordination (rights protection, stable rules) and carry indirect cost (judicial veto of popular legislation); trapped exit means the cost is inescapable. Constitutional critics (moderate, constrained): d ≈ 0.75 — near-target, their preferred readings are structurally excluded from the authoritative legal process, amendment is the only institutional path for override.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not show mandatrophy in the classical sense (founding problem dead, arrangement persists). The founding problem is contested: judicial supremacy advocates claim constitutional stability requires final adjudication; critics claim coordinate or parliamentary readings would preserve stability while restoring legislative authority. However, the constraint shows symptoms of mandate-drift: the founding problem was partly about protecting individual rights against majoritarian legislatures; the modern constraint operates partly to protect judicial authority itself, independent of the rights-protection function. Theater ratio rises (0.28 to 0.41) as doctrinal performance increases to justify expanded scope. The constraint is tangled-rope by classification (genuine coordination + asymmetric extraction), not piton (which would require both beneficiary and payer to be indifferent); but the theater trajectory suggests doctrinal erosion where legitimating narratives carry more weight than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_vs_inherent_authority,
    'Does the constitutional text inherently establish judicial supremacy, or does the text permit (but not require) coordinate-construction or parliamentary-primacy readings?',
    'Originalist textual analysis comparing competing scholarly interpretations of Marbury''s reasoning and the Constitution''s structural language; international comparative evidence of democracies operating under coordinate or parliamentary readings of similarly-drafted texts.',
    'If the text permits alternative readings, judicial supremacy is a constructed institutional choice, not a natural law of constitutionalism — classification shifts to snare (pure extraction) because coordination could be achieved under coordinate or parliamentary readings without veto power. If the text inherently requires judicial supremacy, the coordination function is structure-dependent, and the constraint remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_vs_inherent_authority, conceptual, 'Whether judicial supremacy is textually required or institutionally chosen.').

omega_variable(
    coordination_independence_from_supremacy,
    'Could the coordination functions (stable constitutional meaning, individual-rights protection, executive check) be achieved under a coordinate-construction or parliamentary reading without judicial veto power?',
    'Comparative institutional analysis of Canada (with notwithstanding clause allowing legislative override of rights rulings), Australia (with informal coordinate interpretation), and UK (parliamentary supremacy with human rights protection). Case studies of whether these jurisdictions achieve constitutional stability and rights protection despite absent judicial supremacy.',
    'If coordination is achievable without supremacy, the extraction component is separable and the constraint''s true function is institutional dominance rather than constitutional stability. Classification would trend toward snare. If coordination requires supremacy, the extraction is the necessary price of stability, supporting tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_independence_from_supremacy, empirical, 'Whether constitutional coordination requires judicial veto power.').

omega_variable(
    suppression_mechanism_legal_vs_cultural,
    'Is the measured suppression (0.72) sustained by legal doctrine (courts'' formal authority to nullify acts), or by cultural acceptance of judicial authority (the legislature accepts veto rather than overtly defying courts)?',
    'Historical analysis of moments when legislatures defied courts (e.g., FDR''s court-packing threat, Jackson''s nullification crisis), post-defiance institutional outcomes, and whether defiance was followed by formal override or normalization of the defiant reading.',
    'If suppression is legal-structural, it is robust to cultural change; if suppression is cultural, it is vulnerable to legitimacy loss. A high cultural component would explain why theater ratio rises (enforcement requires increasing rhetorical work) and suggests the constraint is sensitive to narratives about judicial authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_legal_vs_cultural, empirical, 'Whether suppression is enforced by legal doctrine or cultural acceptance.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the judicial-supremacy reading logically foreclose the coordinate-construction reading, or do they coexist as competing live positions held by different institutional actors and schools of thought?',
    'Examine whether scholars and politicians holding coordinate-construction views can maintain their position without internal logical contradiction, and whether courts have ruled coordinate readings textually impossible (rather than merely disfavored). If foreclosure exists, it is via institutional dominance, not logical necessity.',
    'If foreclosure is logical (axiom_contradiction), the reading_relations edge should be ''forecloses''. If coexistence is possible (different parties hold both), the edge should be ''coexists_with''. This determines whether alternative readings remain live institutional options or are shut down by this reading''s structural dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether the judicial-supremacy reading logically forecloses coordinate construction or merely dominates it institutionally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cons_tr_t5, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(cons_tr_t15, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cons_be_t5, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(cons_be_t15, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cons_su_t5, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(cons_su_t15, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(cons_su_t25, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__judicial_supremacy_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional-authority-boundary kernel admits three structurally distinct constraint readings: judicial_supremacy_reading (this constraint, ε=0.68, judiciary beneficiary), coordinate_construction_reading (ε≈0.35, authority distributed), and parliamentary_primacy_reading (ε≈0.50, legislature dominant). Each reading instantiates a different ε because they allocate interpretive authority differently. The three are linked via this network field; each story is ε-invariant within its own reading frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
