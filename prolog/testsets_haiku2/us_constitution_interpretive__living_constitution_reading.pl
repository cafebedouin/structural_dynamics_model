% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Reading: Judicial Meaning-Making via Evolving Interpretation
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'living constitution' reading of the
 *   contested kernel of US constitutional interpretation. The reading holds
 *   that constitutional meaning evolves with societal values and that
 *   judicial interpretive authority derives from reasoned adaptation to
 *   contemporary conditions rather than fidelity to fixed historical meaning.
 *   The constraint operates through federal judicial power to recognize
 *   unenumerated rights (privacy, dignity, autonomy), expand the scope of
 *   enumerated powers (Commerce Clause, Necessary and Proper Clause), and
 *   strike down state laws conflicting with evolved constitutional meaning.
 *   The constraint is claimed as tangled_rope: it coordinates the solution to
 *   the founding problem (how can a fixed historical text govern a changed
 *   world) while extracting authority from states' rights advocates,
 *   original-meaning textualists, and federalism-limiting constituencies. The
 *   constraint generates deep per-seat divergence: from the judiciary's and
 *   civil rights claimants' perspective, it is genuine coordination enabling
 *   constitutional governance to adapt; from originalists' and federalism
 *   advocates' perspective, it is illegitimate judicial overreach
 *   subordinating fixed constitutional law to judicial policy preferences.
 *   The engine computes this divergence from the structural data; the
 *   authored claim and metrics are stated independently.
 *
 * KEY AGENTS:
 *   - Progressive judiciary (federal judges interpreting Constitution as adaptable; institutional power; constrained exit within judicial hierarchy)
 *   - Civil rights expansion claimants (organized beneficiaries; moderate power; constrained exit due to dependence on federal judicial protection)
 *   - States rights advocates (institutional victims; constrained exit from federal constitutional obligation; regulatory autonomy eroded)
 *   - Original-meaning textualists (moderate-power critics; constrained exit from interpretive authority within judiciary; can dissent and advocate amendment)
 *   - Federal regulatory apparatus (institutional beneficiary; trapped exit; dependent on expansive Commerce Clause interpretation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.52).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Reading: Judicial Meaning-Making via Evolving Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '678b41ef-a6b7-4096-a2aa-4ee282a61c0c').
narrative_ontology:cs_kernel_codification('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', fixed_text).
narrative_ontology:cs_authority_grounding('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', lineage).
narrative_ontology:cs_interpretation_layer_present('678b41ef-a6b7-4096-a2aa-4ee282a61c0c').
narrative_ontology:cs_reading_relation('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', foundational, constitutional_meaning_evolves_with_societal_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_societal_values, holdable).
narrative_ontology:cs_axiom_grounding('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', constitutional_meaning_evolves_with_societal_values, instrumental).
narrative_ontology:cs_axiom('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', foundational, judicial_reasoned_elaboration_is_legitimate_meaning_making).
narrative_ontology:cs_axiom_status(judicial_reasoned_elaboration_is_legitimate_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', judicial_reasoned_elaboration_is_legitimate_meaning_making, deontological).
narrative_ontology:cs_reference_frame('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', adaptive_constitutional_interpretation).
narrative_ontology:cs_drift_state('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', contemporary_originalist_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('678b41ef-a6b7-4096-a2aa-4ee282a61c0c', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, progressive_judiciary).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, federalism_limiting_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges—primarily Supreme Court justices and influential circuit court judges—who interpret the Constitution as a living document capable of addressing contemporary social conditions through reasoned elaboration of textual principles. They exercise authority to recognize unenumerated rights (privacy, dignity, autonomy), expand enumerated powers (Commerce Clause), and constrain state lawmaking. Their interpretive method emphasizes the text's general language and the Constitution's adaptive capacity rather than original historical meaning. They are constrained by precedent, by the need to maintain judicial legitimacy, and by political pressure on appointments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, progressive_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Organized social movements and constituencies—African Americans pursuing desegregation and equal protection, women pursuing reproductive autonomy, LGBTQ+ individuals pursuing marriage equality and nondiscrimination—whose claims for rights were not explicitly enumerated in the original Constitution but were recognized as constitutional rights through living-constitution judicial interpretation. They benefit from judicially-recognized protections that state majorities would not voluntarily grant. Their exit is constrained by dependence on federal judicial protection; loss of living-constitution support would expose their rights to state majoritarian rollback.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    organized, generational, constrained, national).

% Individuals and organizations claiming constitutional protection for abortion and contraceptive access. The living-constitution reading recognized privacy as an unenumerated right (Griswold, Roe) through penumbral reasoning. They benefit from judicially-protected reproductive choice. Their exit became mobile post-Dobbs as some migrated to permissive states and others pursued federal statutory protection and state constitutional recognition.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    moderate, biographical, mobile, national).

% LGBTQ+ individuals and advocacy organizations claiming constitutional protection for sexual orientation and gender identity. Living-constitution interpretation enabled recognition of dignity-based rights (Lawrence, Obergefell) and Equal Protection constraints on state discrimination. They benefit from access to civil marriage, freedom from criminalization, and some employment protections. Their exit is constrained by geographic variation in state-level protection and by the vulnerability of federal constitutional protection to originalist reversal.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% The federal executive and legislative branches, including administrative agencies (EPA, OSHA, FDA, HHS) whose regulatory power over economic and social matters is sustained by expansive judicial interpretation of the Commerce Clause and Necessary and Proper Clause. Under living-constitution interpretation, the federal government has regulatory jurisdiction over environmental protection, labor law, healthcare, and civil rights enforcement. Their exit is trapped: they would lose vast jurisdictional authority under originalist constraint of federal powers.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_regulatory_apparatus, beneficiary,
    institutional, generational, trapped, national).

% State governments and federalism-advocates who argue living-constitution interpretation has eroded constitutional limits on federal power and state autonomy. Under originalist constraint, many domains (family law, criminal procedure, commerce regulation, social policy) would remain within exclusive state authority. They bear the cost of constrained regulatory autonomy. Their exit is constrained: states cannot opt out of the federal system or refuse federal constitutional obligations; they can only advocate for amendment or originalist judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    institutional, generational, constrained, national).

% Legal scholars, judges, and political theorists holding originalism as the correct interpretive method. They argue living-constitution interpretation is illegitimate judicial overreach substituting judges' policy preferences for constitutional law. They bear the cost of exclusion from meaning-making authority within the judiciary: their preferred reading remains contested in lower courts but is outnumbered at the Supreme Court level (though recent appointments have shifted composition). Their exit is constrained: they can write dissents, publish scholarship, and advocate for judicial appointments, but cannot directly control constitutional meaning in the short term.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    moderate, generational, constrained, national).

% Individuals in states governed by majorities opposed to federal civil rights norms who are protected by federal judicial constraint on state power (e.g., regarding racial segregation, reproductive access, LGBTQ+ rights). Under an originalist regime that returned regulatory authority to the states, they would lose federal constitutional protection and be subject to majoritarian state law. They bear the risk of living under a regime that could eliminate their constitutional protections. Their exit is trapped: they cannot escape state majoritarian authority except by migration.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federalism_limiting_constituencies, payer,
    powerless, biographical, trapped, national).

% Originalist justices on the Supreme Court who dissent from the living-constitution consensus. They command minority votes on some issues and produce dissents and concurrences that shape legal debate. They are constrained by prior precedent and the difficulty of overruling settled doctrine, though recent appointments have increased originalist strength. They monitor and challenge living-constitution precedent but cannot unilaterally change established law.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, supreme_court_originalist_minority, observer,
    institutional, generational, constrained, national).

% Law professors and legal scholars who theorize about constitutional interpretation and debate the legitimacy and methodology of living-constitutionalism. They influence judicial thinking through scholarship, amicus briefs, and mentorship. They are not party to the constraint's operation but are essential to its legitimation and contestation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, academic_constitutional_lawyers, observer,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, progressive_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework by which the Constitution's fixed textual language can address social conditions, technological developments, and moral understandings unforeseen by the framers without requiring perpetual formal amendment. Coordinates judicial authority with democratic and state actors to interpret constitutional principles (liberty, equality, due process) in light of evolving social values. Enables one historical text to govern a changing society by treating constitutional meaning as capable of principled expansion and reinterpretation.
% TRANSFER_FUNCTION: Transfers regulatory authority from state majorities to the federal judiciary and from enumerated constitutional powers to implicitly inferred powers (Commerce Clause expansion, Necessary and Proper Clause). Transfers authority to define the scope of fundamental rights from original-historical meaning to contemporary values. Concentrates meaning-making power in federal judges who can recognize unenumerated rights, strike down state laws, and constrain state regulatory autonomy in the name of evolved constitutional understanding.
% ABSENT_VOICES: States' rights advocates and original-meaning textualists are structurally excluded from the living-constitution interpretive authority. They have platforms for dissent (Supreme Court dissents, scholarly publishing, political advocacy, Federalist Society), but do not set the framework within which the dominant judiciary interprets the Constitution. Federalism-limiting constituencies in states opposed to civil rights expansion lose voice in state-level policymaking as federal judges constrain state autonomy.
% DISAPPEARANCE_RATIONALE: If the living-constitution reading were abandoned and originalist constraint installed, the functional architecture of federal power would reorganize: (1) the Commerce Clause would narrow, federal regulatory agencies would lose vast jurisdictional authority, federal environmental and labor law would contract; (2) unenumerated rights (privacy, dignity, marriage equality) would lose federal constitutional protection, devolving to state constitutions and federal statute where they would be less secure against majoritarian rollback; (3) the Fourteenth Amendment's reach would contract, reducing federal power to enforce equal protection in the states; (4) civil rights protections would rest on state constitutional law and the political branches, not on federal judicial enforcement. Constitutional litigation would reorganize around what original meaning permits, not what contemporary values suggest. States would recover regulatory autonomy; federal power would narrow.
% FOUNDING_PROBLEM: The Constitution was drafted in 1787-88 by a limited number of white male property owners; it contains no explicit protection for slavery, women's equality, racial equality, privacy, marriage, or many matters modern citizens regard as fundamental. The document uses general language (liberty, due process, equal protection, commerce) that invites interpretation. The founding problem is: how can a fixed historical text, drafted for a vastly different society, govern a modern pluralistic nation with radically different technology, social organization, and moral understanding? How can the Constitution remain law without either becoming obsolete or requiring amendment every generation?
% FOUNDING_PROBLEM_CORROBORATION: Living-constitution advocates argue the founding problem remains live: society continues to change faster than formal amendment is feasible, and the Constitution's textual language (liberty, equal protection, due process) is inherently capable of addressing novel applications through reasoned interpretation. Civil rights movements (African American, feminist, LGBTQ+) attest the problem is live: their claims would never have been explicitly recognized by the 1787 framers, yet modern conscience regards them as fundamental—living-constitutionalism was the mechanism by which these claims gained constitutional recognition. International constitutional courts (Canada, South Africa, European Court of Human Rights) have adopted living-document frameworks, citing the founding problem as justification. Originalist critics (Randy Barnett, Keith Whittington, recent justices Alito and Thomas) argue the founding problem is a false dilemma: the solution is not to change constitutional meaning but to amend it formally, or to constrain judges to original meaning and accept narrow constitutional authority. They argue that frequent amendment (or reliance on the political branches for novel regulation) would restore direct democratic control and the rule of law.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) and rises over the 70-year interval from 0.38 to 0.68. This trajectory reflects three dynamics: (1) the living-constitution reading began as a minority position in mid-20th century and expanded to dominance, shifting from contested to consolidated; (2) as the reading consolidated, it extracted more authority by recognizing unenumerated rights and constraining state power more aggressively (privacy, equal protection, incorporation doctrine); (3) organized resistance (originalism, federalism advocacy) mobilized in response, suggesting the extraction became more visible and contested. Theater ratio rises modestly (0.25 to 0.41) and plateaus: early living-constitutionalism emphasized genuine textual adaptation and principled reasoning; later, a growing share of effort went into rhetorical defense of the reading against originalist critique and into preserving precedent against erosion. Suppression is moderate (0.52) because originalists and federalism advocates maintain institutional platforms, scholarly authority, and political influence; they have not been silenced but have been unable to reverse the interpretive consensus. Resistance is high (0.72): originalism became an organized intellectual and political movement; Federalist Society mobilized resources to place originalist judges; political campaigns turned on constitutional interpretation; post-Dobbs overruling of Roe demonstrates active resistance finally succeeding in displacing a major living-constitution precedent.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme. From the progressive judiciary's position: the living-constitution reading is necessary constitutional interpretation, grounded in the text's open language (liberty, due process, equal protection) and the Constitution's own adaptive logic (amendment clause, framers' general language). From civil rights claimants' position: the reading is the only source of fundamental protection for their dignity and autonomy; without it, their rights would be hostage to state majoritarian will. From states' rights and originalist positions: the reading is judicial overreach, substituting judicial preferences for constitutional law, illegitimately concentrating power in federal judges, and depriving states of sovereign autonomy and citizens of the rule of law. The engine derives directionality from beneficiary/victim declarations and exit options: civil rights claimants sit at low directionality (beneficiaries, trapped exit → subsidized position); progressive judges sit at moderate directionality (beneficiaries of interpretive authority, but constrained by precedent and appointment pressure); originalists sit at high directionality (victims of exclusion from meaning-making, moderate power and constrained exit → targeted position). States sit at high directionality as well (victims of eroded autonomy, institutional power but constrained exit from federal system).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship of each stakeholder to this specific constraint. Progressive judiciary: d ≈ 0.35 (beneficiaries of interpretive authority, organizational power, constrained exit; they set the constraint but are confined by institutional rules and precedent, so not full beneficiaries). Civil rights expansion claimants: d ≈ 0.25 (beneficiaries of recognized rights, but dependent on judicial action; trapped exit because their claim recognition is vulnerable to judicial reversal or originalist reinterpretation; moderate to organized power through advocacy coalitions). Federal regulatory apparatus: d ≈ 0.20 (institutional beneficiary of expansive Commerce Clause interpretation; trapped exit because they would lose jurisdiction under originalist constraint; powerful institutional position but dependent on judicial deference). States' rights advocates: d ≈ 0.70 (victims of eroded state autonomy; institutional power but constrained exit from federal system; they can advocate for constitutional amendment or originalist judicial appointments but cannot exit). Original-meaning textualists: d ≈ 0.65 (victims of exclusion from meaning-making authority within judiciary; moderate power and constrained exit from interpretive framework; they have scholarly and political platforms but do not control judicial interpretation). Federalism-limiting constituencies: d ≈ 0.60 (victims of the risk that state majorities could restrict their rights if living-constitutionalism collapsed; powerless position, trapped exit, dependent on federal protection). These directionalities derive from the beneficiary/victim declarations, power atoms, exit options, and scope; the engine computes χ (effective extraction) from d, and the engine's classification of per-seat type follows from χ, power, and scope combined.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was live at the constraint's inception (t=0): the Constitution is a fixed historical text and societal conditions change; living-constitutionalism offered a solution. At interval midpoint (t=35), the founding problem remains contested: living-constitution advocates argue it is still live (societal change continues, formal amendment is too slow); originalists argue the solution is not judicial adaptation but formal amendment or originalist constraint (which forces amendment for novel conditions, restoring direct democratic control). At interval end (t=70), the founding problem's status is ambiguous: post-Dobbs overruling of Roe demonstrates that living-constitution precedent is vulnerable to institutional displacement; the constraint persists because the interpretive consensus persists, but the consensus is actively contested by organized originalism. Mandatrophy would arise if the founding problem died (constitutional meaning became somehow fixed, or amendment became rapid enough to eliminate lag) while the constraint persisted. Signs of early mandatrophy: (1) theater_ratio rising (effort shifts from genuine textual adaptation to defensive legitimation of the reading); (2) high resistance (originalism mobilized and increasingly successful); (3) post-Dobbs overruling (a major living-constitution precedent was displaced by originalist coalition, suggesting the constraint's institutional foundation is more fragile than appeared). The constraint does NOT yet exhibit mandatrophy because the founding problem remains live and the progressive judiciary still commands a majority; but the trajectory suggests vulnerability to mandatrophy if the originalist coalition consolidates further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the living-constitution reading logically foreclose the originalist reading within a single constitutional framework, or do they coexist as alternative but coherent interpretive methodologies?',
    'Philosophical analysis of whether the two readings'' core premises (meaning evolves vs. meaning fixed at ratification) are contradictory or merely incompatible—a reading difference rather than a logical contradiction. Expert testimony from constitutional theorists holding each view on whether they believe the sibling reading is logically foreclosed or merely false.',
    'If foreclosed: the engine reclassifies the sibling reading as internally incoherent, not merely contested. If coexist: both readings remain live alternatives held by different institutional actors and scholarly communities. The classification affects the type computed for the originalist reading and the termination state predicted for the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationship between living-constitution and originalist readings of the same kernel.').

omega_variable(
    extraction_vs_coordination_ambiguity,
    'Is the living-constitution reading''s extraction (federal authority over state law, judicial meaning-making power, unelected judges constraining majoritarian regulation) the cost of genuine coordination (enabling constitutional meaning to address novel social conditions), or is the coordination function separable from the extraction mechanism?',
    'Empirical examination of whether the founding problem (fixed text + changed world) could be solved without concentrating meaning-making authority in federal judges—e.g., through more frequent constitutional amendment, through originalist constraint requiring amendment to address novel conditions, or through collaborative interpretation involving multiple institutional actors. Analysis of whether coordination function and extraction mechanism are structurally inseparable or only contingently bundled.',
    'If inseparable: the measured extraction is the price of genuine coordination; if separable, the extraction is pure overlay. A framework that solved the founding problem with less concentrated federal judicial power (e.g., living-constitutionalism with greater deference to state variation or political branch interpretation) would lower effective extraction without losing coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_ambiguity, empirical, 'Whether judicial meaning-making power is structurally necessary for constitutional adaptation or contingently bundled with the solution.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) structural—enforced by doctrine, precedent, and institutional hierarchy—or internalized—originalists and federalism advocates accept the living-constitution reading as legitimate even when they disagree with it?',
    'Historical analysis of originalist and federalism advocacy: do they accept living-constitution precedent as binding law (internalized suppression) or do they resist it as illegitimate and demand institutional reform (structural suppression)? Survey of scholarly resistance, political mobilization for originalist judges, and constitutional amendment proposals as evidence of suppression mechanism.',
    'If internalized: originalist resistance is muted and the constraint persists by normative acceptance, not by active suppression. If structural: the living-constitution reading persists despite active resistance and requires ongoing institutional defense (strategic appointments, precedent preservation, rhetorical legitimation). Post-Dobbs overruling of Roe v. Wade suggests structural suppression: originalists did not internalize Roe as legitimate but mobilized institutional power to overturn it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of originalist resistance is structural or internalized.').

omega_variable(
    beneficiary_identity_fusion,
    'Do civil rights expansion claimants fuse their identity with the living-constitution reading in such a way that exit (moving to an originalist interpretive regime) would dissolve their constitutional personhood, or are their rights portable across interpretive methodologies?',
    'Post-Dobbs empirical evidence: LGBTQ+ rights advocates and reproductive autonomy advocates have demonstrated whether they transfer allegiance to alternative constitutional framings (state constitutions, federal statute, international human rights) or whether losing living-constitution support for their rights is experienced as catastrophic loss of personhood and dignity. Interview and affinity data from civil rights communities on whether their political identity is fused to living-constitutionalism or instrumentally attached.',
    'If identity-fused: civil rights claimants experience living-constitution interpretation as part of their constitutional belonging; exit from living-constitutionalism would be experienced as a form of erasure. If identity-portable: civil rights claims can migrate to alternative constitutional and statutory framings and maintain continuity. Identity fusion increases the constraint''s effective suppression on beneficiary seats and increases their exposure to mandatrophy (beneficiaries would bear costs if living-constitutionalism collapsed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, empirical, 'Identity fusion of civil rights beneficiaries to living-constitution interpretive framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__living_constitution_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__living_constitution_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__living_constitution_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__living_constitution_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__living_constitution_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(us_c_tr_t70, us_constitution_interpretive__living_constitution_reading, theater_ratio, 70, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(us_c_be_t70, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(us_c_su_t70, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 70, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__living_constitution_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The us_constitution_interpretive kernel decomposes into three constraint stories representing three competing readings of how constitutional meaning is determined. (1) living_constitution_reading (this constraint) holds meaning evolves with societal values via judicial adaptation; epsilon=0.68, beneficiaries=civil rights claimants and federal regulatory apparatus, extracted authority from states and originalists. (2) originalist_reading holds meaning is fixed at ratification; epsilon would be substantially lower (constraint on judicial power, not expansion). (3) popular_constitutionalism_reading holds meaning shaped by democratic contestation, not solely judicial interpretation; epsilon and beneficiary structure would differ. The kernel itself is the commitment to written constitutional law; the readings instantiate different constraints on how that law's meaning evolves. Each story is ε-invariant and structurally complete; sibling readings are not observables within one constraint but separate constraints linked by network relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
