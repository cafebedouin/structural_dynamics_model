% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation: Meaning Fixed at Ratification
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   Constitutional originalism proposes that the Constitution's meaning is
 *   fixed at the moment of ratification (or at the moment of each amendment's
 *   ratification), and judges are bound to interpret it according to the
 *   public meaning of its text at that historical moment, not according to
 *   contemporary social values or evolving understandings. This is one
 *   reading of the US Constitution as a kernel—a persisting commitment whose
 *   authority grounds itself in formal ratification but whose interpretation
 *   is contested. The originalist reading claims that meaning is fixed and
 *   judges are constrained; contemporary rights claimants claim that meaning
 *   must evolve or that historical evidence is indeterminate. The originalist
 *   reading structures the constraint as a tangled rope because it serves a
 *   genuine coordination function (unified judicial methodology) while
 *   simultaneously extracting from those whose rights claims lack historical
 *   support. Originalist judges are simultaneously beneficiaries (the
 *   constraint frees them from accusations of activism by making their
 *   interpretations appear determined by history) and constrained seats (when
 *   historical evidence points toward outcomes they oppose, the constraint
 *   binds them too). This asymmetry—genuine coordination plus asymmetric
 *   extraction—is the hallmark of tangled rope structure.
 *
 * KEY AGENTS:
 *   - originalist_judges: institutional power; constrain constitutional interpretation to historical public meaning at ratification; benefit from constraint's legitimacy when outcomes align with preferences, accept constraint when outcomes diverge
 *   - contemporary_rights_claimants_without_historical_basis: moderate power; excluded from constitutional protection by lack of historical grounding; pay through foreclosure of novel rights claims and forced resort to amendment or legislation
 *   - counter_majoritarian_constraint_advocates: organized power; benefit from originalism's framing as a legitimate brake on majoritarian overreach; defend the constraint even when historical evidence produces outcomes they oppose
 *   - living_constitutionalist_judges: institutional power; suppressed by originalist constraint; forced to either adopt originalist arguments or dissent from originalist precedent
 *   - legislative_branch: analytical observer; gains power when originalism narrows constitutional constraints, faces limits when originalism expands historical rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.71).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Constitutional Interpretation: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, 'e0fabb9c-f076-409f-b9aa-80fcd361629f').
narrative_ontology:cs_kernel_codification('e0fabb9c-f076-409f-b9aa-80fcd361629f', fixed_text).
narrative_ontology:cs_authority_grounding('e0fabb9c-f076-409f-b9aa-80fcd361629f', lineage).
narrative_ontology:cs_interpretation_layer_present('e0fabb9c-f076-409f-b9aa-80fcd361629f').
narrative_ontology:cs_reading_relation('e0fabb9c-f076-409f-b9aa-80fcd361629f', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e0fabb9c-f076-409f-b9aa-80fcd361629f', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('e0fabb9c-f076-409f-b9aa-80fcd361629f', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('e0fabb9c-f076-409f-b9aa-80fcd361629f', meaning_fixed_at_ratification, empirically_contingent).
narrative_ontology:cs_axiom('e0fabb9c-f076-409f-b9aa-80fcd361629f', foundational, historical_public_meaning_determines_interpretation).
narrative_ontology:cs_axiom_status(historical_public_meaning_determines_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('e0fabb9c-f076-409f-b9aa-80fcd361629f', historical_public_meaning_determines_interpretation, empirically_contingent).
narrative_ontology:cs_axiom('e0fabb9c-f076-409f-b9aa-80fcd361629f', secondary, contemporary_values_irrelevant_to_meaning).
narrative_ontology:cs_axiom_status(contemporary_values_irrelevant_to_meaning, holdable).
narrative_ontology:cs_axiom_grounding('e0fabb9c-f076-409f-b9aa-80fcd361629f', contemporary_values_irrelevant_to_meaning, deontological).
narrative_ontology:cs_reference_frame('e0fabb9c-f076-409f-b9aa-80fcd361629f', framers_intent_doctrine).
narrative_ontology:cs_drift_state('e0fabb9c-f076-409f-b9aa-80fcd361629f', contemporary_contested_interpretation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0fabb9c-f076-409f-b9aa-80fcd361629f', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, federalist_institutional_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, contemporary_rights_claimants_without_historical_basis).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, majoritarian_policy_expansion_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_constraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, originalist_jurisprudence).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, framers_intent_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate constitutional disputes by consulting 1788/amendment-date historical public meaning. The constraint frees them from the accusation of judicial activism while delegating policy choices to the political branches. Their authority derives from appearing bound by discoverable historical fact rather than contemporary judgment. When originalism produces outcomes they favor, they justify those outcomes as constrained; when it produces outcomes they oppose, they maintain the methodology's legitimacy anyway. Exit would mean abandoning the interpretive framework and accepting accountability for value judgments in judicial opinions.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, originalist_judges, beneficiary).

% Defend originalism as the only legitimate brake on majoritarian overreach. They argue that without fixed meaning anchored in the ratified text, judges become legislators and the Constitution becomes a tool of momentary political will. They benefit from the constraint's framing because it legitimizes legal limits on democratic outcomes—which is exactly what they want when those limits block policies they oppose. If originalism lost judicial adoption they would seek alternative constraint mechanisms or shift to political process arguments. Their commitment is to constraint itself, not necessarily to originalism's specific outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, civilizational, mobile, national).

% Seek judicial recognition of constitutional rights—privacy, dignity, equal protection expansions, due process in novel contexts—that lack direct 1788/amendment-date textual or historical support. The originalist constraint forecloses their claims unless they can reconstruct historical support (labor-intensive, unlikely to succeed for novel contexts). They can amend the Constitution but face the super-majoritarian amendment threshold. They can lobby legislatures but lack the counter-majoritarian shield a court-recognized constitutional right provides. Exit would mean abandoning the Constitution as a tool for their agenda and accepting that rights not historically recognized remain unprotected.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, contemporary_rights_claimants_without_historical_basis, payer,
    moderate, biographical, constrained, national).

% Believe constitutional meaning evolves with social understanding and contemporary circumstances. The originalist constraint actively suppresses their interpretive methodology and forces them to either adopt originalist arguments to win cases or dissent. They are structurally disadvantaged when originalist judges control the bench and can enforce originalist doctrine as binding precedent. Exit would mean leaving the bench, which sevens them from institutional power.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, living_constitutionalist_judges, excluded).

% Seek to expand constitutional protections for contemporary concerns (climate, digital privacy, corporate personhood limitations, healthcare access) through court litigation. Originalism forecloses novel rights claims and shifts their efforts toward amendment (extremely difficult) or legislation (easily reversible by future majorities). They benefit from living constitutionalism but pay the cost of originalist constraint when it blocks their preferred outcomes. They can adapt by reframing claims in historical terms or by political mobilization; they are not trapped, but constrained.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, majoritarian_policy_expansion_movements, payer,
    powerful, biographical, mobile, national).

% Originalism vindicates the interpretive practice of reconstructing framers' intent and 1788 public meaning. Historians and originalist legal scholars benefit professionally and intellectually from the constraint's legitimacy because their expertise becomes judicially relevant. Non-agents (the practice itself, the doctrine) are included here for narrative completeness.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, framers_historical_intent_interpreters, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_meaning__originalist_reading, framers_historical_intent_interpreters).

% Controls whether originalism functions as binding constitutional law. A 5-4 or 6-3 originalist majority can enforce the constraint; a living-constitutionalist majority can erode it by distinguishing or limiting originalist precedents. Exit would mean losing institutional power; staying locked in originalism constrains their own flexibility when historical evidence points toward outcomes they oppose.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, supreme_court_majority_coalition, agenda_setter,
    institutional, civilizational, trapped, national).

% Observes how originalism allocates which issues remain within legislative discretion (contemporary policy questions) versus which are constitutionalized (questions with 1788/amendment historical basis). When originalism narrows constitutional protections, legislatures gain power to set policy; when originalism expands historical rights, legislatures face constitutional limits. They have analytical standing but do not control the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legislative_branch, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originalism coordinates judicial decision-making around a fixed interpretive methodology—consulting historical public meaning at ratification—rather than allowing each judge to update constitutional meaning with contemporary social understanding. This solves the coordination problem of what counts as legitimate constitutional interpretation: judges apply the same constraint (historical fidelity) instead of diverging on whether contemporary morality should shape meaning.
% TRANSFER_FUNCTION: Transfers interpretive authority from living judges' assessment of contemporary values to dead framers' demonstrable public meaning at ratification. It moves policy-setting power from courts (which living constitutionalism would grant) to legislatures and amendment processes (where originalism leaves it). For rights claimants lacking historical support, it transfers the burden of constitutional justification from 'does this matter today?' to 'did the framers intend this?'—a much harder burden.
% ABSENT_VOICES: Living constitutionalist judges and contemporary rights claimants without historical basis would object strongly if present and heard. They would argue that constitutional meaning must evolve and that locking interpretation to 1788 understanding freezes power relations from 235 years ago into contemporary law. They are systematically excluded by the original interpretive methodology itself—living constitutionalism is explicitly ruled out as illegitimate by the constraint's logic.
% DISAPPEARANCE_RATIONALE: If originalism disappeared overnight and living constitutionalism became the governing framework, constitutional law would expand dramatically: privacy rights, equal protection across novel domains, due process protections in contemporary contexts, and unenumerated rights would all become live constitutional questions. Federal legislative power would contract where judges read new constitutional limits into the 14th Amendment, and state legislatures would face new constitutional constraints. The entire allocation of which policy questions are constitutional versus political would shift.
% FOUNDING_PROBLEM: After 1800, courts faced interpretive chaos: judges disagreed radically on whether the Constitution's meaning changed with social evolution or remained fixed at ratification. Some judges treated the Constitution as a living document, others as a fixed text; some deferred to framers' intent, others to contemporary moral judgment. This produced inconsistency, accusations of judicial willfulness, and uncertainty about whether courts were constrained by law or simply imposing preferences. Originalism proposes that the founding problem is solved by fixing meaning at the ratification moment: all judges work from the same historical data, producing constraint and legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and judges attest the founding problem of interpretive chaos is solved by originalism's constraint. Living constitutionalist judges attest the founding problem persists under originalism (judges still disagree on what historical evidence means, still exercise judgment, still produce divergent outcomes) and is worse under originalism because the constraint is false—judges pretend to be bound by history while actually choosing between competing historical interpretations. Constitutional historians attest that the historical evidence is genuinely contested and does not resolve close cases; multiple reasonable historical reconstructions exist. Political scientists attest that originalism has not eliminated judicial disagreement on controversial cases. The problem's status remains contested across all non-originalist seats.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderately high (0.68 by interval end) because the constraint systematically forecloses rights claims that lack 1788 historical support, transferring interpretive authority from contemporary judgment to dead framers' intent. This is extraction from those claimants because the cost of justification is much higher under originalism than under living constitutionalism. Suppression is high (0.71) because the constraint actively suppresses non-originalist judicial methodologies: originalism is presented as the only legitimate interpretive approach, and living constitutionalist dissents are characterized as judicial activism. Theater is moderate-low (0.28) because originalism does involve genuine interpretive work (reconstructing historical evidence), but a growing share of the constraint's operation defends against the claim that historical evidence is indeterminate and that judging requires contemporary value judgments. The measurement series show gradual increases in all three metrics over the 40-year interval, reflecting increased polarization around originalism, greater clarity about its extractive effects on certain rights claimants, and increased reliance on historical narrative to defend outcomes that align with originalist judges' preferences. The time horizon is civilizational (constitutive of how judges interpret for generations) while the biographical horizon for rights claimants reflects the generational stakes—one denied right can persist for a lifetime of that claimant.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judge's seat and the contemporary rights claimant's seat should diverge sharply in computed type. From the originalist judge's position, originalism is a genuine coordination mechanism solving judicial chaos and producing constraint; the extraction is incidental to the coordinating function. From the rights claimant's position, originalism is pure foreclosure dressed in historical narrative; no coordination benefit accrues to them, only cost. Living constitutionalist judges occupy a third position: they see originalism as false constraint (judges still exercise judgment, still disagree) and extraction-plus-suppression (their methodology is illegitimized, their dissents dismissed as activism). The engine computes these divergences from the structural data: power, exit options, beneficiary/victim status, and role. No override is needed; the structural differences map naturally to divergent type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges benefit from the constraint's legitimacy while being constrained by its historical fidelity requirement (d~0.40-0.50, near-symmetric). Counter-majoritarian advocates benefit substantially without the constraint binding them (they choose which outcomes they prefer, then invoke or downplay originalism accordingly—d~0.10-0.20, beneficiary end). Contemporary rights claimants without historical support are near-full targets: the constraint systematically forecloses their claims and offers no exit; they are identity-locked into their claim (cannot change the historical status of what they seek) and trapped in the legal system's refusal to protect it (d~0.85-0.95, target end). Living constitutionalist judges are suppressed targets (constrained by originalist precedent, unable to adopt their preferred methodology—d~0.75-0.85). The engine's directionality computation derives d from beneficiary/victim declarations and exit options; the asymmetry across seats produces per-seat type classifications: originalist judges compute as tangled-rope beneficiaries (coordinated via shared methodology, extracting from targets); contemporary rights claimants compute as snare victims (trapped, identity-locked, no coordination benefit); counter-majoritarian advocates compute as rope beneficiaries (genuine coordination benefit, no extraction cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is interpreted by originalists as: 'How can courts be legitimately constrained?' Answer: 'By fixing meaning at ratification.' Originalism claims to have solved this problem by providing a constraint mechanism that appears mechanical and historical rather than discretionary. However, the mandatrophy question is: 'Has the founding problem that justified originalism as a solution become dead while the constraint persists?' The living constitutionalist objection is exactly this: the founding problem (interpretive chaos and judicial willfulness) is NOT solved by originalism because (1) historical evidence is contested and does not determine outcomes, (2) judges still disagree on constitutional meaning, and (3) the claim of historical constraint is theatrical—judges selecting friendly historical evidence and dismissing adverse evidence as not truly public meaning. If this objection is correct, originalism is a zombie constraint: it persists by enforcing historical narrative while the problem it purports to solve is unsolved. The intermediate position is that originalism partially solves the problem (provides some structure, reduces but does not eliminate judicial disagreement) and partially substitutes a new problem (forecloses contemporary rights claims without historical grounding, freezes power relations from 1788). The six_questions response coded founding_problem_status as contested, reflecting that the founding problem remains live for originalists, dead (or unsolved) for living constitutionalists. Theater is rising (0.28 by interval end) as the constraint increasingly relies on rhetorical framing (citing framers' intent, historical evidence) rather than mechanical determination of outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_determinacy,
    'Does historical evidence about 1788 public meaning of the Constitution actually determine the outcomes originalism predicts, or is the historical evidence sufficiently contested that judges exercise substantive discretion in interpreting it?',
    'Empirical examination of originalist court decisions: do judges reach the same outcomes when historical evidence is genuinely indeterminate? Do originalist judges diverge from each other on contested historical questions? Does originalist method predict outcomes better than other methods? Comparative analysis of originalist vs. living constitutionalist judges on the same cases.',
    'If historical evidence is genuinely determinative, originalism solves the constraint problem and is correctly classified as coordination-plus-some-extraction (tangled rope). If historical evidence is contested and judges still exercise discretion, originalism is largely theater and should be reclassified toward snare (pure extraction dressed as constraint). If judges partially constrain themselves via originalism but still exercise discretion, it remains tangled rope but with higher theater_ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_evidence_determinacy, empirical, 'Whether originalist method actually constrains judges or is a cover for discretionary judgment.').

omega_variable(
    founding_problem_persistence,
    'Has originalism solved the founding problem (judicial chaos and willfulness), or does the problem persist under originalism while being reframed as disagreement about historical meaning?',
    'Measure divergence in outcomes on controversial cases before and after originalism''s adoption; measure whether originalist judges converge on controversial historical questions; assess whether originalism has reduced or maintained the perception of judicial activism.',
    'If originalism solved the problem, it should remain tangled rope. If the problem persists unsolved, originalism is zombie-constraint (mandatrophy resolved: constraint persists despite its founding justification becoming obsolete) and should be reclassified toward piton (inertial maintenance without functional justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem originalism was built to solve is live, dead, or persists under reframing.').

omega_variable(
    reading_foreclosure_claim,
    'Does originalism logically foreclose living constitutionalism (as originalists claim—that living constitutionalism is not a legitimate interpretive methodology), or do the readings coexist as genuinely alternative commitments?',
    'Examine whether the core premises of originalism (meaning fixed at ratification) and living constitutionalism (principles endure, applications evolve) are logically contradictory within a single framework, or whether they represent different normative choices about what should ground constitutional authority.',
    'If foreclosure is real (originalism''s premise logically rules out living constitutionalism''s core), the reading_relations entry for living_constitutionalist_reading should be forecloses. If the readings represent different normative commitments that coexist (different parties hold different views, neither logically rules out the other), the relation should be coexists_with. This affects how the engine models the kernel structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_claim, conceptual, 'Whether originalism and living constitutionalism are logically exclusive or genuinely coexisting readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of non-originalist interpretive methodologies primarily structural (originalists control the bench and can enforce precedent, so dissenters are legally bound) or internalized (legal academics and judges internalize originalism as the legitimate methodology, making alternative views feel improper even absent judicial hierarchy)?',
    'Post-bench-shift suppression trajectory: if non-originalist judges gain a majority and living constitutionalism re-emerges as legitimate methodology, suppression was primarily structural (reversed by institutional change). If originalism persists as an intellectual norm even among non-originalist judges, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—judges carry originalist constraints in their thinking even absent institutional binding. This would increase the chi computation. If structural, the suppression effect reverses when institutional control changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of non-originalist interpretation is structural or internalized.').

omega_variable(
    identity_lock_for_rights_claimants,
    'Are contemporary rights claimants genuinely identity-locked (unable to change which right they seek because it flows from their identity: women seeking reproductive autonomy, LGBTQ+ people seeking equal dignity, etc.), or is their identity-lock merely sociological (they could reframe their claims, though doing so would require disowning part of their identity)?',
    'Examine whether rights claimants can successfully reframe claims in historical terms to satisfy originalism, or whether the historical reframing requires abandoning the contemporary understanding of the right that motivated the claim in the first place. Measure whether rights claimants accept originalist constraint or mobilize for amendment/legislative remedy.',
    'If identity-locked at the sociological level (reframing would require identity-disowning), the exit_options classification as identity_locked is correct and d remains near 0.9 (target end). If reframing is possible without identity-disowning, exit is less trapped and d should be lower (constrained rather than identity_locked).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_for_rights_claimants, empirical, 'Depth of identity-lock for contemporary rights claimants in originalist constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_meaning__originalist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t8, observed).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_meaning__originalist_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t16, observed).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_meaning__originalist_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t24, observed).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_meaning__originalist_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(us_c_tr_t32, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t8, us_constitution_meaning__originalist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(us_c_be_t8, observed).
narrative_ontology:measurement(us_c_be_t16, us_constitution_meaning__originalist_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(us_c_be_t16, observed).
narrative_ontology:measurement(us_c_be_t24, us_constitution_meaning__originalist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(us_c_be_t24, observed).
narrative_ontology:measurement(us_c_be_t32, us_constitution_meaning__originalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(us_c_be_t32, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(us_c_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t8, us_constitution_meaning__originalist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(us_c_su_t8, observed).
narrative_ontology:measurement(us_c_su_t16, us_constitution_meaning__originalist_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(us_c_su_t16, observed).
narrative_ontology:measurement(us_c_su_t24, us_constitution_meaning__originalist_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(us_c_su_t24, observed).
narrative_ontology:measurement(us_c_su_t32, us_constitution_meaning__originalist_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(us_c_su_t32, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(us_c_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% The constraint us_constitution_meaning is decomposed into three readings: originalist (this file), living_constitutionalist, and positivist. Each instantiates a different constraint because the three readings embed different ε values (originalism's ε is high because it forecloses novel rights; living constitutionalism's ε is lower because it opens application to contemporary needs; positivism's ε depends on how formal procedures are used). The readings share a kernel (the Constitution) but produce different extraction profiles. All three readings are linked via network.affects_constraints to mark their family membership. The originalist reading influences both siblings: it sets the contemporary benchmark for what counts as legitimate interpretation, so living constitutionalism and positivism define themselves partly against originalism's claims. Living constitutionalism forecloses originalism's core premise (that meaning is fixed at ratification) while originalism forecloses living constitutionalism's premise (that contemporary application should evolve). Positivism coexists with both: it focuses on procedures rather than meaning, so it is compatible with originalism's procedural formality but also with living constitutionalism's interpretive flexibility as long as procedures are followed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
