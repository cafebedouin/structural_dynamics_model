% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitution: Evolving Meaning Reading of the 1787 Text
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the living-constitution reading of the 1787
 *   kernel: the position that constitutional meaning legitimately evolves
 *   with societal conditions, and that the text functions as an aspirational
 *   framework whose general clauses (due process, equal protection) are meant
 *   to be filled in by successive generations rather than frozen at
 *   ratification. This reading has produced real coordination value — it let
 *   a text with an all-but-unamendable formal process remain applicable to
 *   unenumerated modern claims (privacy, dignity, evolving family structures)
 *   that the framers had no occasion to address. But it also substitutes the
 *   judiciary and the interpretive academy for the amendment process the text
 *   itself specifies for updating, layering an asymmetric extraction (from
 *   originalist litigants, state legislatures with divergent local
 *   majorities, and Article V's institutional stakeholders) onto that
 *   coordination function. The story treats the living reading strictly as it
 *   is, on its own account of the standing constitutional arrangement — it
 *   does not import the originalist or positivist readings' verdicts, and it
 *   does not treat the living reading's OWN endorsed outcome as the ε
 *   referent (per the kernel-reading rule, ε is authored for the arrangement
 *   under contest, not the reading's preferred destination).
 *
 * KEY AGENTS:
 *   - federal_judiciary: administers the living-reading doctrine, decides which social changes count as constitutionally settled
 *   - constitutional_law_academy: theorizes and professionally sustains the methodology
 *   - marginalized_rights_claimants: primary beneficiaries of doctrine reaching unenumerated claims
 *   - originalist_litigants and state_legislatures_with_diverging_norms: bear the cost of unpredictable doctrinal shifts and preempted local majoritarian outcomes
 *   - democratic_amendment_process_stakeholders: bear the diffuse cost of Article V being functionally bypassed
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
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitution: Evolving Meaning Reading of the 1787 Text").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '00891d45-2663-4cdd-80b0-de790978f254').
narrative_ontology:cs_kernel_codification('00891d45-2663-4cdd-80b0-de790978f254', fixed_text).
narrative_ontology:cs_authority_grounding('00891d45-2663-4cdd-80b0-de790978f254', lineage).
narrative_ontology:cs_interpretation_layer_present('00891d45-2663-4cdd-80b0-de790978f254').
narrative_ontology:cs_reading_relation('00891d45-2663-4cdd-80b0-de790978f254', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('00891d45-2663-4cdd-80b0-de790978f254', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('00891d45-2663-4cdd-80b0-de790978f254', foundational, text_as_aspirational_framework_not_fixed_code).
narrative_ontology:cs_axiom_status(text_as_aspirational_framework_not_fixed_code, holdable).
narrative_ontology:cs_axiom_grounding('00891d45-2663-4cdd-80b0-de790978f254', text_as_aspirational_framework_not_fixed_code, conventional).
narrative_ontology:cs_axiom('00891d45-2663-4cdd-80b0-de790978f254', foundational, judicial_recognition_of_evolved_consensus_is_legitimate_updating_authority).
narrative_ontology:cs_axiom_status(judicial_recognition_of_evolved_consensus_is_legitimate_updating_authority, holdable).
narrative_ontology:cs_axiom_grounding('00891d45-2663-4cdd-80b0-de790978f254', judicial_recognition_of_evolved_consensus_is_legitimate_updating_authority, instrumental).
narrative_ontology:cs_reference_frame('00891d45-2663-4cdd-80b0-de790978f254', aspirational_framework_updated_by_consensus).
narrative_ontology:cs_drift_state('00891d45-2663-4cdd-80b0-de790978f254', contemporary_post_dobbs_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('00891d45-2663-4cdd-80b0-de790978f254', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, constitutional_law_academy).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, marginalized_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, progressive_legislative_coalitions).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_litigants).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_legislatures_with_diverging_norms).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, democratic_amendment_process_stakeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal appellate and Supreme Court judges apply the living-constitution methodology to derive rights and structural doctrines not explicit in the 1787 text (privacy, dignity, evolving standards of decency), treating the document as a framework whose application shifts with societal consensus as they perceive it. They administer the doctrine case by case and control which social changes count as sufficiently settled to warrant new constitutional readings.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars and law schools build careers, casebooks, and clerkship pipelines around interpretive theories that treat the text as an aspirational framework requiring ongoing scholarly elaboration. Their professional and reputational capital is invested in the continued plausibility and prestige of evolving-meaning methodology.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_law_academy, beneficiary,
    organized, generational, mobile, national).

% Individuals and groups whose claims (privacy, reproductive autonomy, marriage equality, dignity-based protections) find no explicit textual anchor in 1787 language depend on the living reading to obtain judicial recognition at all. They cannot exit the constitutional system and have no other forum with comparable authority to vindicate these claims quickly.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, marginalized_rights_claimants, beneficiary,
    powerless, biographical, trapped, national).

% Political coalitions that cannot secure formal Article V amendments (a supermajority process) achieve durable policy outcomes faster by persuading courts that societal norms have already evolved, substituting judicial doctrine for the amendment process when amendment is politically infeasible.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, progressive_legislative_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Parties who structured their legal claims or defenses around the text's original public meaning find courts instead consulting contemporary values, producing outcomes they experience as unpredictable and post hoc. They can appeal or seek certiorari but cannot opt out of a judiciary that has adopted the living methodology as controlling law.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_litigants, payer,
    moderate, biographical, constrained, national).

% State legislatures that reflect local majoritarian preferences at odds with the federal judiciary's read of 'evolving standards' have their statutes invalidated on constitutional grounds even where no textual provision or amendment addresses the issue. Their recourse is limited to slow-moving federal appointments processes or long-shot constitutional amendments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_legislatures_with_diverging_norms, payer,
    powerful, generational, constrained, regional).

% Citizens and factions who believe constitutional change should occur only through Article V's supermajority amendment process see that process bypassed when courts declare that meaning has already 'evolved,' effectively substituting five-justice majorities for the constitutionally specified amendment threshold. They bear the cost of a degraded amendment pathway whose formal difficulty no longer tracks the actual difficulty of constitutional change.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_amendment_process_stakeholders, payer,
    organized, civilizational, trapped, national).

% Adherents of the fixed-meaning reading are present in the judiciary and academy but structurally excluded from setting the interpretive baseline whenever the living methodology commands a judicial majority; their objection that this substitutes contemporary elite consensus for law is registered in dissents but does not control outcomes in living-reading eras.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_reading_proponents, excluded,
    organized, civilizational, analytical, national).

% Historians and comparative constitutionalists study how the living-reading doctrine has actually been applied across decades, tracing which social-consensus claims proved durable and which were later reversed, without a stake in the doctrine's continuation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a two-centuries-old text with a near-impossible formal amendment threshold to remain governance-relevant by permitting judicial updating in response to changed social conditions, avoiding the alternative of either constitutional rupture or frozen 18th-century governance.
% TRANSFER_FUNCTION: Moves interpretive authority from the amendment process (Article V, requiring broad democratic supermajorities) to the federal judiciary and the professional class that theorizes for it; moves substantive policy outcomes toward whichever coalition can persuade five justices that societal norms have shifted, rather than whichever coalition can win a constitutional supermajority.
% ABSENT_VOICES: Originalist judges, historians of original public meaning, and state legislative majorities whose preferences diverge from the judiciary's read of contemporary consensus are present in dissent and academic minority positions but do not control the doctrine's application when a living-reading majority holds the bench.
% DISAPPEARANCE_RATIONALE: If the living-constitution methodology vanished overnight and courts reverted strictly to originalist or positivist textualism, decades of doctrine grounded in unenumerated rights (substantive due process privacy claims, dignity-based equal protection extensions) would lose their interpretive foundation, forcing either their re-derivation from other doctrinal sources, legislative codification, or formal Article V amendment — a substantial reorganization of constitutional law and the coalitions built around it.
% FOUNDING_PROBLEM: The 1787 text uses general, often unelaborated language (due process, equal protection, unreasonable searches) applied to social conditions the framers could not have anticipated (electronic surveillance, reproductive technology, changing family structures), and the Article V amendment process is so difficult that formal textual updating rarely keeps pace with social change.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside the U.S. living-constitutionalism tradition (citing more easily amendable constitutions elsewhere) attest that the underlying problem — textual rigidity outpacing social change — is real and general, not invented by the living-reading school. However, originalist scholars and several sitting jurists dispute that judicial interpretation is the legitimate remedy for that rigidity, arguing the difficulty of Article V is a deliberate design feature rather than a defect to be worked around; no source entirely outside both the living-reading academy and its intended beneficiaries corroborates that judicial updating specifically (as opposed to amendment, legislation, or restraint) is the correct response.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42) and rising slowly across the 1954-2024 interval: the doctrine genuinely resolves an otherwise-intractable textual-rigidity problem (coordination function), but its expanding application increasingly substitutes for what the amendment process would otherwise decide, which is where the asymmetric cost to originalist litigants and diverging state legislatures lives. Suppression (0.38) reflects that the doctrine's persistence depends on judicial willingness to treat prior 'evolved' holdings as binding precedent, which forecloses relitigating the underlying methodology question in ordinary cases — a real but moderate suppressive mechanism, not an overwhelming one, since originalist challenges to specific doctrines remain a live and often successful litigation strategy at the Supreme Court. Theater ratio (0.3) captures that a portion of 'evolving standards' reasoning in opinions functions rhetorically to legitimate outcomes reached on other grounds, without fully replacing substantive doctrinal work.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary and the constitutional law academy sit closest to the beneficiary end: they administer and professionally sustain the methodology and are not themselves subject to its constraints. Marginalized rights claimants and progressive coalitions are structural beneficiaries in the sense that the doctrine is often the only viable path to their preferred outcomes, though they bear no cost from the doctrine's operation as such. Originalist litigants, state legislatures with divergent local majorities, and Article V stakeholders sit toward the target end: their claims are overridden or their preferred amendment pathway is functionally bypassed by judicial declaration that meaning has already shifted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — textual rigidity outpacing social change given an all-but-unamendable formal process — remains genuinely live (contested, not dead): the U.S. amendment rate has been near zero for decades while social conditions have changed substantially, so the coordination function the living reading claims to serve is not obsolete. This prevents mislabeling the reading as pure extraction dressed as coordination. At the same time, the reading's own account does not establish that judicial interpretation specifically is the correct or exclusive remedy for that rigidity (as opposed to legislative codification or a genuinely reformed amendment process), which is where the asymmetric cost to non-beneficiary seats persists structurally rather than being a temporary transitional cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_reading_committer_structure,
    'This constraint is one reading (living_reading) of the contested kernel us_constitution_1787. The sibling readings — originalist_reading (meaning fixed at ratification) and positivist_reading (text-plus-amendments, judicially constrained to text) — are separate constraints with their own ε, beneficiaries, and victims. Where is the actual disagreement located structurally?',
    'The disagreement is located at the interpretive-authority-source axis: living_reading locates legitimate updating authority in judicial recognition of evolved societal consensus; originalist_reading locates it exclusively in original public meaning at ratification; positivist_reading locates it in enacted text plus formal Article V amendment, treating judicial interpretation as bound to text. Resolving which reading should control constitutional practice is not an empirical question resolvable by evidence internal to any one reading — it is a contested normative-cum-institutional-design question about where interpretive authority ought to sit.',
    'If the positivist_reading''s framework were adopted as controlling, most of this constraint''s beneficiary set (federal_judiciary as agenda_setter, constitutional_law_academy) would lose their present interpretive latitude, and unenumerated-rights doctrine built under living_reading would require re-derivation from text or formal amendment. If originalist_reading''s framework were adopted, the living_reading''s entire beneficiary structure collapses and its victims become the new baseline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_reading_committer_structure, conceptual, 'Committer-frame declaration: this story is one reading of the us_constitution_1787 kernel; the sibling readings are separate constraints, and the disagreement is located at the source-of-interpretive-authority axis, not resolvable within this reading alone.').

omega_variable(
    elite_capture_of_evolving_norms,
    'Is the judiciary''s identification of an ''evolved societal consensus'' a genuine tracking of broad social change, or is it frequently a capture mechanism where a narrower elite professional and judicial consensus is characterized as broader societal evolution?',
    'Comparative analysis of doctrines the Court characterized as reflecting ''evolving standards'' against independently measured public opinion data at the time of the ruling (polling, referenda, state legislative counts) — divergence between claimed societal consensus and measured public opinion would support the capture reading.',
    'If capture is frequent, effective extraction is higher than the authored 0.42 and the beneficiary set should be understood as narrower (judiciary/academy specifically) rather than broadly representing marginalized claimants'' genuine democratic support. If capture is rare, the coordination framing is better supported and current ε is closer to accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Whether ''evolving societal consensus'' as identified by courts tracks genuine broad social change or professional/judicial elite consensus.').

omega_variable(
    amendment_process_bypass_severity,
    'To what extent does living-reading jurisprudence function as a de facto substitute for Article V amendment, versus filling genuine textual gaps the amendment process was never expected to address?',
    'Case-by-case doctrinal history distinguishing rulings that address genuinely unanticipated technological/social conditions (e.g., electronic surveillance under the Fourth Amendment) from rulings that resolve contested value questions the amendment process was specifically designed to adjudicate (e.g., questions with active, organized amendment campaigns underway).',
    'Higher bypass severity supports weighting the victim-side cost (to democratic_amendment_process_stakeholders) more heavily and would push suppression and extractiveness upward; lower severity supports the coordination-function framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_bypass_severity, empirical, 'Whether living-reading doctrine substitutes for or supplements the Article V amendment process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_1787__living_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(us_c_tr_t1968, us_constitution_1787__living_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(us_c_tr_t1982, us_constitution_1787__living_reading, theater_ratio, 1982, 0.24).
narrative_ontology:measurement(us_c_tr_t1996, us_constitution_1787__living_reading, theater_ratio, 1996, 0.26).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__living_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__living_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1954, us_constitution_1787__living_reading, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(us_c_be_t1968, us_constitution_1787__living_reading, base_extractiveness, 1968, 0.33).
narrative_ontology:measurement(us_c_be_t1982, us_constitution_1787__living_reading, base_extractiveness, 1982, 0.37).
narrative_ontology:measurement(us_c_be_t1996, us_constitution_1787__living_reading, base_extractiveness, 1996, 0.39).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__living_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__living_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1954, us_constitution_1787__living_reading, suppression_requirement, 1954, 0.25).
narrative_ontology:measurement(us_c_su_t1968, us_constitution_1787__living_reading, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement(us_c_su_t1982, us_constitution_1787__living_reading, suppression_requirement, 1982, 0.32).
narrative_ontology:measurement(us_c_su_t1996, us_constitution_1787__living_reading, suppression_requirement, 1996, 0.34).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__living_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__living_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the us_constitution_1787 kernel, decomposed per the epsilon-invariance principle: living_reading (this story), originalist_reading (meaning fixed at ratification), and positivist_reading (text-plus-amendments, judicial interpretation bound to text). Each reading has its own ε, beneficiary/victim structure, and claimed type because each identifies a structurally distinct arrangement as the object of evaluation, not merely a different opinion about the same arrangement. living_reading is authored here as a tangled_rope (real coordination function meeting a real textual-rigidity problem, layered with asymmetric extraction from originalist litigants and Article V stakeholders); the sibling stories should be consulted for their own independently authored ε and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
