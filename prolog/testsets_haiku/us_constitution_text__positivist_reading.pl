% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Constitutional Positivist Reading: Formal Enactment Validity
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The positivist reading of US constitutional validity holds that a
 *   constitutional claim is valid if and only if it traces to formally
 *   enacted text (the Constitution as written and amended through Article V
 *   procedure). Validity does not depend on moral content, historical intent,
 *   or contemporary justice—it depends on procedural pedigree. This reading
 *   solves the interpretive problem of what binds judges: the formal text and
 *   the formal amendment process, not judges' evolving moral views or
 *   historical claims. The reading's beneficiaries are institutional
 *   stability and rule-of-law predictability; its victims are substantive
 *   justice claims that cannot anchor to the text, and historically
 *   marginalized groups who were excluded from the original enactment. The
 *   tension this generates is the constraint: the reading coordinates on a
 *   procedure for validity, but that very procedure forecloses claims that
 *   lack textual foundation, even meritorious ones. This is measured as a
 *   tangled rope—genuine coordination function (common baseline for
 *   constitutional argument) bundled with asymmetric extraction (judges
 *   constrained from remedying injustice without amendment; marginalized
 *   groups unable to appeal to constitutional morality).
 *
 * KEY AGENTS:
 *   - Positivist legal theorists: agenda-setter role, define the reading and its boundaries
 *   - Federal judiciary: constrained payer role, must enforce textual constraint despite substantive tension
 *   - Substantive justice seekers: payer role, foreclosed from constitutional remedies lacking textual anchor
 *   - Historically marginalized groups: powerless payer role, identity-locked to the jurisdiction, excluded from the original text that binds them
 *   - Institutional stability defenders: beneficiary role, gain predictable, rule-bound constitutional adjudication
 *   - Originalist and living constitutionalist jurists: excluded role, contest the positivist frame from within constitutional interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.62).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Constitutional Positivist Reading: Formal Enactment Validity").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc').
narrative_ontology:cs_kernel_codification('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', fixed_text).
narrative_ontology:cs_authority_grounding('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', lineage).
narrative_ontology:cs_interpretation_layer_present('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc').
narrative_ontology:cs_reading_relation('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', foundational, formal_procedure_determines_validity).
narrative_ontology:cs_axiom_status(formal_procedure_determines_validity, holdable).
narrative_ontology:cs_axiom_grounding('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', formal_procedure_determines_validity, conventional).
narrative_ontology:cs_axiom('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', foundational, article_v_amendment_exclusivity).
narrative_ontology:cs_axiom_status(article_v_amendment_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', article_v_amendment_exclusivity, conventional).
narrative_ontology:cs_reference_frame('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', formal_enactment_legitimacy).
narrative_ontology:cs_drift_state('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ed22f92-ef0d-45ea-9afb-899e6d0e2fbc', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, institutional_stability_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, predictability_dependent_actors).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_seekers).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, historically_marginalized_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, federal_judiciary).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, rule_of_law_formalism).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, constitutional_amendment_supremacy).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, judicial_role_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, legislators, executive officials, and scholars who benefit from predictable constitutional interpretation locked to formal enactment text and procedure. Their position is that constitutional meaning is what the text says as formally enacted; allowing judges to reinterpret based on evolving morality or historical claims destabilizes institutional legitimacy and rule of law. They do not collect rents but do collect institutional authority and interpretive deference from this reading.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, institutional_stability_defenders, beneficiary,
    institutional, generational, analytical, national).

% Articulate and defend the positivist reading: constitutional validity tracks formal enactment (ratification through Article V amendment process), not moral content, historical intent, or living adaptation. They set the interpretive frame through scholarship, judicial opinions, and law school pedagogy. They administer the boundary between valid (formally enacted) and invalid (substantive claims lacking formal enactment) constitutional claims.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, positivist_legal_theorists, agenda_setter,
    organized, generational, analytical, national).

% Must enforce the positivist reading through judicial review: declining to adjudicate substantive justice claims on constitutional grounds unless those claims are grounded in formally enacted text. They bear the cost of rejecting meritorious-seeming claims that lack textual anchor, and the institutional cost of defending unpopular outcomes that the text technically permits. At the same time, they administer enforcement of the reading through interpretation and precedent.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, federal_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, federal_judiciary, agenda_setter).

% Parties seeking constitutional remedies for injustice, discrimination, or rights violations on substantive grounds (moral claims, evolving social understanding, historical wrongs) that lack explicit textual foundation in the formal enactment. Under the positivist reading, their claims are categorically foreclosed unless they can be reframed as textual interpretation—a constraint they cannot exit without constitutional amendment (Article V) or waiting for the judiciary to reinterpret (non-exiting paths for most claimants).
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, substantive_justice_seekers, payer,
    moderate, biographical, constrained, national).

% People systematically excluded from the original enactment (women, enslaved people, non-property-holders, racial minorities) cannot easily appeal to the positivist reading for remedy because the formal text that excluded them is also what binds the constraint. Their moral claims for historical correction are categorically outside the frame: the reading accepts the text as written, not as it should have been written. They cannot exit the jurisdiction; their identity as citizens is what makes the constraint bind.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, historically_marginalized_groups, payer,
    powerless, biographical, identity_locked, national).

% Jurists who argue that the original public understanding at ratification governs meaning. They would object that the positivist reading ignores the semantic content the enactors themselves intended, collapsing validity into mere formal procedure and losing the constraint that original meaning supplies. They are not in the positivist frame but contest it from within constitutional interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, originalist_judges, excluded,
    institutional, generational, constrained, national).

% Scholars and judges who argue constitutional meaning evolves with society and that formalism deadlocks the system against necessary adaptation. They would testify that the positivist reading makes the Constitution rigid and prevents it from serving its adaptive function, forcing substantive reform onto the political branches when the judiciary could interpret dynamically. They are excluded from the positivist frame but actively contest its premises.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, living_constitutionalist_jurists, excluded,
    institutional, generational, constrained, national).

% The formal amendment process (Article V: 2/3 Congress + 3/4 state ratification) is the sole mechanism for constitutional change under the positivist reading. This gatekeeping function is both a feature (stability, deliberation) and a constraint (nearly impossible supermajority requirement makes amendment the hardest law to change). This mechanism's role as THE valid path for constitutional change is what the positivist reading enforces.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, amendment_process_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).

% Law professors, judges, practicing attorneys, and constitutional scholars who study and debate the reading's coherence, implications, and fit with American jurisprudence. They are neither benefiting nor paying (they are observers), but they generate the discourse that legitimates or challenges the positivist framing.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_interpretive_community, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, institutional_stability_defenders).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of what counts as binding law in a written-constitution system: every legal actor (judges, legislators, citizens) coordinates on the rule that constitutional validity derives from formal enactment procedure, not from posterior moral evaluation or textual reinterpretation. This creates predictable, common-knowledge baseline for constitutional argumentation.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual judges and moral philosophers to the formal text and the Article V amendment process. Substantive justice claims that cannot anchor to the text transfer from the judiciary into the political branches, where they must win formal amendment or statutory change. The constraint moves authority AWAY from outcome-sensitive interpretation and TOWARD procedure-bound interpretation.
% ABSENT_VOICES: Historically excluded parties (women, enslaved people, property-less citizens, racial minorities at the time of enactment) are structurally absent from the legitimacy base of the formal text they are now bound by. Their objection—that a text written by and for a narrow subset of the population should not govern those it excluded—cannot be heard within the positivist frame because the frame accepts the text as-is. Originalists and living constitutionalists would also object that the positivist reading evacuates semantic meaning and evolutionary purpose.
% DISAPPEARANCE_RATIONALE: If the positivist reading disappeared and were replaced by outcome-sensitive or historically-sensitive interpretation, constitutional law would reorganize: judges would have much broader authority to reframe claims in light of contemporary justice standards or original intent; substantive justice claims would no longer be categorically foreclosed; the Article V amendment process would become less functionally critical as a legitimacy requirement. The entire structure of American constitutional adjudication rides on some account of validity; the positivist reading is ONE such account, and losing it would force a wholesale reorganization of the judiciary's interpretive constraints.
% FOUNDING_PROBLEM: The Constitution is a fixed text enacted in 1787 and amended through a specified process (Article V). A framework for legal interpretation must address: what makes a constitutional claim valid? Is it the text's formal enactment status, its original meaning, its contemporary application, its moral content, or something else? The positivist reading solves this by tying validity solely to formal enactment procedure—the text is binding because it went through Article V ratification, regardless of its content or how times have changed.
% FOUNDING_PROBLEM_CORROBORATION: The positivist reading's account of the founding problem is attested by legal positivists, rule-of-law formalists, and institutional-constraint theorists. However, originalists attest that the founding problem is ALSO about recovering semantic content (what the text MEANT at enactment); living constitutionalists attest that the problem is ALSO about enabling constitutional meaning to serve an adaptive function as society evolves. Jurisprudential history (law review scholarship, Supreme Court divided opinions over centuries) and comparative constitutional law (other democracies' approaches to written constitutions) confirm that the 'what makes a claim valid?' problem is real and differently answered across interpretive traditions. The positivist answer is ONE coherent solution; the contest is real, not a manufactured ambiguity.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.48 (moderate: the positivist reading does provide genuine coordination value through shared baseline, but it also forecloses substantive claims with no formal anchor) and rises to 0.62 (by contemporary era, decades of jurisprudence have accumulated cases where the reading's foreclosure of non-textual claims becomes more salient—marginalized groups' historical claims, evolving rights claims, have all been categorically rejected for lack of textual foundation). Theater ratio is low-moderate (0.12→0.31): the formalism is functionally real (judges do genuinely constrain themselves to the text and Article V), but an increasing share of the reading's persistence is performative—recurrent invocation of 'just following the text' while actively interpreting what the text means, what counts as 'the text,' and how formal amendment is practically impossible. Suppression is moderate (0.42→0.58): the reading relies partly on structural foreclosure (substantive justice claims cannot be made in constitutional court; they must go to political process) and partly on active judicial enforcement (judges actively refusing to extend constitutional protection to new claims). The trajectory shows growing extractiveness and theater as the reading ages: the coordination value remains steady, but the cost to non-beneficiaries accumulates, and the reading's maintenance increasingly requires performative insistence that judges are 'just following the text' when their own interpretive choices are doing real work. Measurements are placed at ~47-year intervals to track major historical shifts (ratification era, Reconstruction, progressive era, civil rights, contemporary).
 *
 * PERSPECTIVAL GAP:
 *   The positivist legal theorist and institutional-stability defender seats compute the reading as valid rope—genuine coordination, mutual benefit through predictability, no serious costs. The federal judiciary seat experiences tangled rope—they genuinely benefit from having clear, textual, defensible constraints on their power, but they bear the cost of rejecting meritorious claims and defending the boundary even when doing so seems unjust. The substantive justice seeker and historically marginalized group seats experience this as a snare—they are foreclosed from even making certain arguments in court, and the foreclosure is defended as inevitable procedure rather than choice. The originalist and living constitutionalist seats would classify this as a false summit (a constraint presented as natural procedure that is actually a constructed reading that advantages certain outcomes). These divergences should compute clearly from the structural data: the payer seats have constrained exit (they cannot opt out of federal jurisdiction or the Constitution's binding force), while the beneficiary seats have analytical exit (they can choose to defend or refute the reading intellectually). The suppression score reflects that substantive claims face structural barriers plus active enforcement: they are not merely foreclosed in principle but actively rejected in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Institutional stability defenders are the structural beneficiaries—they collect lower litigation risk, clearer boundaries, and institutional legitimacy from the reading. They have analytical exit: they can choose to adopt a different reading. d ≈ 0.1–0.2 (near beneficiary end). Federal judiciary sits at d ≈ 0.4–0.5 (symmetric): they gain interpretive clarity and institutional constraint (a feature they want, to legitimate their review power), but they pay by foreclosing remedies and defending unpopular textual outcomes. Substantive justice seekers are constrained payers with moderate power: they want constitutional remedies but the reading forecloses them unless they can reframe as textual interpretation. d ≈ 0.7 (toward target end). Historically marginalized groups are the canonical targets: they are powerless, identity-locked (citizenship makes the jurisdiction inescapable), and the very text that binds them excluded them from authorship. Their directionality should be near 1.0 (maximum target). The suppression score is high for these seats because their barriers to exit are multiple: geographic, political (they cannot easily constitute a new polity), and identity-fused (their status as citizens is what makes the Constitution apply). The reading does not scale suppression by scope or power—suppression is structural (the foreclosure is structural, not amplified). But directionality does scale effective extraction: the powerless, identity-locked seats have higher χ from the same ε than mobile, powerful seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading's mandate is: Constitutional validity derives from formal enactment (Article V ratification), not from moral content or interpretive evolution. The founding problem was: What makes a constitutional claim valid in a written-constitution system with a fixed text and difficult amendment process? The positivist answer is coherent: procedure is the anchor, not content or intent or morality. However, mandatrophy emerges when examining whether the reading's survival depends on the mandate or on the extraction it enables. The reading persists partly because it coordinates on a real problem (how should judges be constrained?) and partly because it forecloses challenges to structural inequalities written into the original text. The mandate alone ("procedure determines validity") is genuine and lived. But the reading's persistence ALSO depends on suppressing non-textual justice claims—which suggests that if the mandate became impossible (e.g., if Article V itself were amended to enable easier constitutional change, or if judges adopted living constitutionalism), the reading would lose a major source of its power. The mandatrophy verdict is UNRESOLVED: the reading is not dead (it remains a live jurisprudential position), but it is contested (originalists and living constitutionalists offer competing mandates), and the persistence of the positivist reading against those competitors depends not just on the mandate but on institutional power (which readings judges adopt, which ones law schools teach). Theater-ratio rise (0.12→0.31) suggests increasing performativity: the reading is maintained partly by the scaffold of 'just following the text' even as interpretive choices do real work. This is consistent with a reading that has solved a coordination problem but now persists partly due to path dependence and extraction benefit—a tangled rope verging on piton. The theater rise does NOT yet warrant a piton classification because the reading's fundamental function (constraining judges to formal procedures) is still operationally real, not merely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_tension,
    'Is the positivist reading a genuine constraint on judges, or is it a reading that reads itself as constraint? Does formal procedure bind independently, or does the reading bind by being the judges'' chosen interpretive frame?',
    'Comparative analysis: if judges in jurisdictions with different constitutional traditions apply formal-procedure constraints without accepting positivist framing, the constraint is structural (procedure binds independently). If formal procedure only constrains where judges adopt the positivist reading, the reading is the constraint, not the procedure itself.',
    'If the reading IS the constraint (not the procedure), then the reading is contingent on institutional adoption and could be replaced. If the procedure binds independently, the reading is expressing something real about written constitutions in general, not just American jurisprudence. This affects whether the reading can be changed by judges (adoption choice) or whether it is structurally imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_tension, conceptual, 'Whether formal procedure is a structural constraint or a reading-contingent interpretive frame.').

omega_variable(
    historical_exclusion_paradox,
    'How can the positivist reading claim to bind historically excluded groups (women, enslaved people, minorities) who were not parties to the original enactment and who explicitly rejected when they were admitted? Does the reading''s legitimacy hold for those who were excluded from authorship?',
    'Jurisprudential examination of whether exclusion affects the binding force of formal enactment. Comparative analysis of how other democracies treat constitutions enacted by limited electorates. Philosophical analysis of whether consent is retroactively applied to heirs of excluded people.',
    'If exclusion undermines the binding force, the reading must explain how legitimacy carries across generations and demographic shifts. If it does not, the reading''s universality is compromised, and the extraction from historically marginalized groups becomes explicit structural targeting. This affects whether the reading can be said to coordinate on a shared baseline (if some parties never consented, the baseline is not shared).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusion_paradox, conceptual, 'Whether formal enactment''s binding force is compromised when the enactment excluded those it now governs.').

omega_variable(
    amendment_impossibility_cycle,
    'The Article V amendment process is practically impossible (requires 2/3 Congress + 3/4 states, no supermajority can sustain). Does the positivist reading''s reliance on Article V as the sole path for constitutional change make substantive reform structurally unavailable, and does this entrench injustices the reading forecloses from judicial remedy?',
    'Historical measurement: count amendments since 1789 and their timing relative to major justice movements. Causal analysis: where political reform succeeded despite constitutional barriers (civil rights statutes, statutory remedies), did the positivist reading''s foreclosure of judicial constitutional remedies increase the burden on political process?',
    'If Article V impossibility creates a structural trap—substantive injustices can neither be remedied by courts (positivist foreclosure) nor by constitutional amendment (practical impossibility)—then the reading enables a particular form of entrenchment. This would support the interpretation that the reading extracts from marginalized groups by closing both the judicial and constitutional remedy paths, leaving only ordinary legislation. This would strengthen the snare classification from the payer perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_impossibility_cycle, empirical, 'Whether the positivist reading''s mandatory route through Article V creates a structural trap for substantive reform.').

omega_variable(
    formalism_vs_interpretive_choice,
    'To what extent does the positivist reading''s insistence on ''just following the text'' evacuate judges'' inevitable interpretive choices? If judges must still interpret what the text means and what counts as ''the text,'' is the formalism hiding rather than eliminating judicial discretion?',
    'Doctrinal analysis: trace how the ''neutral'' formal approach produces different outcomes depending on what judges count as ''the text'' (original framing, historical amendments, structural inference, etc.). Measure whether judge-created law (common law constitutional reasoning) under the positivist frame differs predictably from explicitly outcome-oriented interpretation.',
    'If formalism masks discretion rather than eliminating it, the theater-ratio metric''s rise is explained—the reading''s increasing performativity reflects that judges are doing interpretive work while insisting they are merely following formal procedure. This supports the tangled-rope classification: genuine coordination function (shared commitment to textual anchor) alongside hidden extraction (judges retain discretion while claiming not to). If formalism genuinely constrains discretion, the theater-ratio rise would need a different explanation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalism_vs_interpretive_choice, empirical, 'Whether formal procedure genuinely eliminates judicial discretion or merely redirects and hides it.').

omega_variable(
    reading_sibling_foreclosure_structure,
    'Does the positivist reading logically foreclose its sibling readings (originalist and living constitutionalist), or do all three readings coexist as live positions held by different judicial and scholarly camps?',
    'Examine whether a single judge or court can hold positivist commitments AND originalist commitments (e.g., ''validity derives from formal procedure AND we must recover original meaning''). If coherent integration is possible, the readings coexist. If holding both requires explicit contradiction, the readings foreclose each other.',
    'If coexistence is possible, all three readings are live interpretive frames competing for institutional adoption—none is foreclosed by logical contradiction. This affects how the kernel contest is modeled: it becomes an institutional choice about which reading to adopt, not a logical determination of which reading survives. If foreclosure is strict, the sibling readings must be modeled as mutually exclusive live options with one reading''s adoption requiring another''s rejection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure_structure, conceptual, 'Whether the positivist reading logically forecloses originalist and living constitutionalist readings or coexists with them as distinct institutional choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t47, us_constitution_text__positivist_reading, theater_ratio, 47, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t47, observed).
narrative_ontology:measurement(us_c_tr_t94, us_constitution_text__positivist_reading, theater_ratio, 94, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t94, observed).
narrative_ontology:measurement(us_c_tr_t141, us_constitution_text__positivist_reading, theater_ratio, 141, 0.26).
narrative_ontology:measurement_basis(us_c_tr_t141, observed).
narrative_ontology:measurement(us_c_tr_t188, us_constitution_text__positivist_reading, theater_ratio, 188, 0.3).
narrative_ontology:measurement_basis(us_c_tr_t188, observed).
narrative_ontology:measurement(us_c_tr_t235, us_constitution_text__positivist_reading, theater_ratio, 235, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t235, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t47, us_constitution_text__positivist_reading, base_extractiveness, 47, 0.54).
narrative_ontology:measurement_basis(us_c_be_t47, observed).
narrative_ontology:measurement(us_c_be_t94, us_constitution_text__positivist_reading, base_extractiveness, 94, 0.58).
narrative_ontology:measurement_basis(us_c_be_t94, observed).
narrative_ontology:measurement(us_c_be_t141, us_constitution_text__positivist_reading, base_extractiveness, 141, 0.61).
narrative_ontology:measurement_basis(us_c_be_t141, observed).
narrative_ontology:measurement(us_c_be_t188, us_constitution_text__positivist_reading, base_extractiveness, 188, 0.62).
narrative_ontology:measurement_basis(us_c_be_t188, observed).
narrative_ontology:measurement(us_c_be_t235, us_constitution_text__positivist_reading, base_extractiveness, 235, 0.62).
narrative_ontology:measurement_basis(us_c_be_t235, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t47, us_constitution_text__positivist_reading, suppression_requirement, 47, 0.47).
narrative_ontology:measurement_basis(us_c_su_t47, observed).
narrative_ontology:measurement(us_c_su_t94, us_constitution_text__positivist_reading, suppression_requirement, 94, 0.51).
narrative_ontology:measurement_basis(us_c_su_t94, observed).
narrative_ontology:measurement(us_c_su_t141, us_constitution_text__positivist_reading, suppression_requirement, 141, 0.55).
narrative_ontology:measurement_basis(us_c_su_t141, observed).
narrative_ontology:measurement(us_c_su_t188, us_constitution_text__positivist_reading, suppression_requirement, 188, 0.58).
narrative_ontology:measurement_basis(us_c_su_t188, observed).
narrative_ontology:measurement(us_c_su_t235, us_constitution_text__positivist_reading, suppression_requirement, 235, 0.58).
narrative_ontology:measurement_basis(us_c_su_t235, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The positivist reading is one of three structurally distinct interpretations of the US Constitution as a kernel. The three readings (positivist, originalist, living constitutionalist) instantiate different constraints because they have different beneficiary/victim structures, different enforcement mechanisms, and different ε values. The positivist reading constrains judges to formal procedure (Article V, ratified text); originalism constrains judges to original public meaning; living constitutionalism constrains judges to adaptive interpretation. These are not angles on one constraint—they are three different constraints with the same referent (the Constitution). The positivist reading's ε measures how much the formal-procedure constraint extracts from substantive justice seekers and marginalized groups; other readings' ε values would measure different extraction structures. Linked through network.affects_constraints because the positivist reading's adoption affects whether originalist and living constitutionalist readings are live options in judicial practice: if positivism becomes the only accepted reading, originalism and living constitutionalism lose institutional seats; if pluralism persists, all three remain live.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__positivist_reading, powerless, 0.95).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
