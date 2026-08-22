% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Doctrine
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   Originalism as a constitutional interpretation doctrine claims that
 *   constitutional meaning is fixed at ratification and judges must recover
 *   the original public understanding of the text at that historical moment.
 *   The doctrine emerged in the 1980s as a conservative response to Warren
 *   Court activism, positioning itself as a neutral constraint on judicial
 *   discretion. However, the structural operation differs from the stated
 *   justification: extractiveness has increased monotonically over 45 years
 *   as originalist judges use the methodology to foreclose contemporary
 *   rights claims while treating historical practices favoring the status quo
 *   as binding law. The suppression of adaptive interpretation has
 *   intensified particularly after 2010, with the Bush/Trump judicial
 *   appointments consolidating originalist dominance and narrowing the range
 *   of acceptable constitutional argument. Theater ratio has grown as the
 *   justificatory narrative (constraining judges) increasingly serves as
 *   cover for substantive outcomes that favor conservative policy even when
 *   the historical evidence is contested or ambiguous.
 *
 * KEY AGENTS:
 *   - Originalist judges (institutional authority, agenda-setters): define operative constitutional meaning through opinions, set precedent, enforce methodological gatekeeping
 *   - Conservative legal establishment (organized beneficiary): Federalist Society, Republican appointees, conservative think tanks; derives institutional dominance and policy victories from originalist suppression of rights claims
 *   - Contemporary rights claimants (powerless, trapped victims): seek constitutional protection for privacy, equality, workplace protections, digital rights; foreclosed by originalism's historical burden requirement
 *   - Non-originalist judges and legal scholars (moderate, constrained): live constitutionalists, purposivists, progressive scholars; marginalized when originalists control the bench
 *   - Historical evidence practitioners (organized beneficiary): historians, constitutional scholars, archivists whose expertise creates the foundation for 'original meaning' claims; benefit from institutional demand for their work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.79).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Constitutional Interpretation Doctrine").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '3d07e80d-7a2d-42f3-99fe-e85636aa3cda').
narrative_ontology:cs_kernel_codification('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', fixed_text).
narrative_ontology:cs_authority_grounding('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', extraction).
narrative_ontology:cs_interpretation_layer_present('3d07e80d-7a2d-42f3-99fe-e85636aa3cda').
narrative_ontology:cs_reading_relation('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', foundational, judges_bound_by_historical_evidence).
narrative_ontology:cs_axiom_status(judges_bound_by_historical_evidence, holdable).
narrative_ontology:cs_axiom_grounding('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', judges_bound_by_historical_evidence, instrumental).
narrative_ontology:cs_reference_frame('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', text_plus_historical_public_meaning_1787_1868).
narrative_ontology:cs_drift_state('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d07e80d-7a2d-42f3-99fe-e85636aa3cda', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_establishment).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, contemporary_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_interpretation_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, historical_evidence_practitioners).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, non_originalist_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution through the lens of original public meaning at ratification. They set precedent, write doctrinal opinions, and enforce originalist methodology through judicial review and citation gatekeeping. They argue this constrains judicial discretion and anchors law in the written document. They actively defend originalism against living constitutionalism by dismissing post-ratification practice as irrelevant unless it evidences original meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, analytical, national).

% Derives institutional dominance, policy victories, and legitimacy claims from originalist doctrine. Includes conservative judges (Federalist Society members), Republican appointees, conservative think tanks (Heritage Foundation, American Enterprise Institute), law school networks. Benefits from the constraint's suppression of rights claims not grounded in 18th/19th century practice, which tends to preserve existing social arrangements and limit regulatory authority. Has significant resources to develop originalist legal theory, fund judicial appointments, litigate strategic cases, and shape legal scholarship.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_establishment, beneficiary,
    organized, generational, arbitrage, national).

% Seek constitutional protections for rights not explicitly recognized or defended at 1787-1868: privacy rights, workplace discrimination protections, reproductive autonomy, LGBTQ+ equality, digital privacy, access to justice. Under originalism, their claims are foreclosed unless they construct a historical pedigree at ratification. They bear the cost of burden-shifting: must prove historical grounding rather than relying on constitutional principle, contemporary circumstances, or evolving moral understanding. They cannot exit because constitutional rights are the legal infrastructure of their basic freedoms and survival; alternative remedies (legislative protection) are weaker and subject to majoritarian reversal.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, contemporary_rights_claimants, payer,
    powerless, immediate, trapped, national).

% Legal scholars (living constitutionalists, purposivists, pragmatists), non-originalist judges, civil rights organizations, and progressive law professors argue that constitutional principles must evolve to address contemporary circumstances and moral understanding. They are excluded from judicial authority when originalists hold the bench majority. They bear the cost of suppressed interpretive methodology: their scholarship is marginalized as 'activism,' their students face barriers to prestigious clerkships, their interpretive framework is delegitimized. They can publish scholarship and lobby for judicial appointments, but their interpretive methodology is systematically disfavored in courts controlled by originalist majorities.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, adaptive_interpretation_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, adaptive_interpretation_advocates, excluded).

% Non-agent entity: the historical actors (Constitutional Convention delegates, state ratifying conventions) whose intent originalism purports to recover. Their words, practices, and historical context are the constraint's referent and nominal authority source. Listed as beneficiary because their historical arrangements (slavery, limited suffrage, property-based rights) are preserved by originalism's refusal to adapt constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, framers_and_ratifiers, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_text__originalist_reading, framers_and_ratifiers).

% Historians, constitutional scholars, law and history specialists, archivists, and paleographers who specialize in 18th and 19th century history and recover evidence of original meaning. They benefit from the constraint because it creates substantial institutional demand for their expertise: funded research positions, law school employment, litigation consulting, prominent publication opportunities in prestigious law reviews. Their work defining what counts as 'original public understanding' shapes the constraint's actual operation. They have some mobility: could shift to other historical periods or methodologies if originalism's demand collapsed, but currently benefit significantly from originalism's dominance.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, historical_evidence_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Congress is excluded from the interpretive process itself but structurally constrained by originalist judicial interpretation of its enumerated powers. Originalism often restricts congressional authority to narrowly construed enumerated powers, limiting legislative flexibility to respond to contemporary problems. Congress cannot redefine constitutional meaning directly (short of constitutional amendment, which requires supermajority consensus). It can respond with legislation that works within originalist constraints, propose constitutional amendments, or file amicus briefs attempting to influence judicial interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, legislative_branch, excluded,
    institutional, generational, constrained, national).

% Judges who accept living constitutionalism, purposivism, textualism without historical anchoring, or other non-originalist methodologies. They bear the cost of institutional marginalization when originalists control the bench. Their opinions are overruled or distinguished; their methodology is treated as illegitimate judicial activism by originalist colleagues and in legal discourse. They can write dissents and produce persuasive scholarship, but cannot set binding precedent or shape judicial doctrine when originalists hold institutional majority power.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, non_originalist_judges, payer,
    moderate, biographical, constrained, national).

% Political movements, coalitions, and organizations seeking expansive rights protections, robust regulatory authority for government, and constitutional adaptation to modern realities (environmental law, workplace regulation, healthcare, civil rights, criminal procedure). They are excluded from favorable judicial interpretation under originalism and must pursue constitutional change through constitutional amendment (high barrier), legislative action (hedged by originalist judicial review), or attempting to persuade originalist judges that their preferred outcomes happen to align with original meaning (difficult coordination problem). Faces structural disadvantage in constitutional adjudication under originalism.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, progressive_political_coalition, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_establishment).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, rule-like method for constitutional interpretation that claims to constrain judicial discretion by anchoring judges to historical evidence rather than personal policy preferences or contemporary moral intuitions. Coordinates legal professionals around a shared interpretive methodology, promising to reduce apparent arbitrariness in constitutional adjudication and provide predictability.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary democratic majorities and evolving ethical understanding to 18th/19th century framers' public meaning (text + historical practice). Moves judicial outcomes: favors rights claims historically recognized or grounded in historical practice over contemporary ones; favors structural constitutional limits on legislative authority over rights expansions. Redistributes legal legitimacy: conservative policy positions gain immunity from constitutional challenge if they match historical practice; progressive policy positions require historical grounding to survive originalist review.
% ABSENT_VOICES: Contemporary constituencies seeking constitutional protection for rights not historically grounded (reproductive autonomy, LGBTQ+ equality, digital privacy, workplace regulation, environmental protection) are absent from the interpretive process by methodology design—excluded because they cannot meet the burden of proving historical meaning. Progressive constitutional scholars and living constitutionalist judges are excluded from judicial authority when originalists hold bench power. Legislative majorities cannot directly redefine constitutional meaning; alternative constitutional interpreters (non-originalists) are marginalized. Historians who document that 'original meaning' is contested or constructed are excluded from judicial credibility.
% DISAPPEARANCE_RATIONALE: If originalist doctrine disappeared from federal courts and judges returned to mixed methodologies (living constitutionalism, purposivism, balancing tests, contextual interpretation), constitutional law would immediately expand and restructure: privacy rights would acquire new doctrinal grounding; equal protection and due process would expand beyond historical practice; regulatory authority would broaden; individual liberty claims foreclosed under originalism would become justiciable; the Constitution would be read as evolving. Judicial discretion would reappear as an explicit factor in opinion writing. The conservative legal establishment's recent institutional dominance would weaken substantially. Congressional authority would expand. Rights claimants would win cases that originalism forecloses.
% FOUNDING_PROBLEM: The Warren Court's expansion of individual rights (1950s-60s) and the Burger Court's continued rights protection (1970s), which conservative critics characterized as results-oriented judicial activism where judges imposed their policy preferences under the guise of constitutional interpretation. The founding problem was: how can judges be constrained from imposing personal ideology rather than following law?
% FOUNDING_PROBLEM_CORROBORATION: By 2025, the founding problem has substantially resolved: the Supreme Court's composition has shifted dramatically toward conservative/originalist judges (due to Reagan, Bush Sr., Bush Jr., Trump appointments); the Court no longer pursues expansive rights jurisprudence; Court doctrine has moved toward deference to states and limitations on individual rights (Second Amendment, voting rights, reproductive rights, regulatory authority). The conservative legal establishment (primary originalism architects: Scalia, Bork, Levy, McGinnis, Barnett) attests the founding problem remains live, citing perceived 'judicial activism' and the risk of rights expansion—but this attestation conflicts with observable facts. External corroboration from constitutional scholars (Stephen Breyer, Laurence Tribe, progressives generally) documents that the Warren Court activism problem ended ca. 2005-2010, and the founding problem is now dead: what persists is originalism itself functioning not as a solution to the founding problem but as an institutional lock on conservative outcomes. Academic historiography and comparative jurisprudence (Canadian courts, European courts) document that the founding problem (activist judges imposing preferences) is not uniquely solved by originalism; other methodologies contain judicial discretion equally well or better. The mismatch between conservative attestation and external corroboration is substantial and widening.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at 2025) reflects the doctrine's operation as asymmetric burden-shifting: contemporary rights claimants must prove 18th/19th century grounding; conservative movements can defend existing arrangements by historical reference. The cost to adaptive interpretation advocates is suppressed alternative methodology. Suppression (0.79) is the active enforcement mechanism: originalist judges dismiss non-originalist argument as illegitimate activism, exclude competing interpretive methodologies from serious consideration, and use narrow historical interpretation to foreclose rights expansion. Suppression has intensified with Trump judicial appointments consolidating originalist institutional power (2016-2021). Theater ratio (0.42) reflects growing tension between stated rationale (constraining judges) and actual operation (constraining adaptive interpretation while permitting conservative outcomes even with contested historical support). The upward trajectory of all three metrics over 45 years indicates that extractiveness is accumulating as originalism gains institutional dominance — the constraint is becoming MORE extractive, not less, contrary to its neutrality claims. The shared time grid (1980, 1990, 2000, 2010, 2015, 2020, 2025) tracks the Supreme Court's composition shifts: 1980 pre-Scalia; 1990 post-Scalia emergence; 2000 post-Bush v. Gore originalist momentum; 2010-2015 consolidation; 2020-2025 Trump appointments locking originalist control.
 *
 * PERSPECTIVAL GAP:
 *   FROM ORIGINALIST JUDGES' SEAT: the constraint is a genuine solution to the founding problem (judicial activism). Judges perceive themselves as bound by historical evidence and constrained from policy-making. The methodology feels neutral and text-driven. Exit appears impossible absent constitutional amendment. FROM CONTEMPORARY RIGHTS CLAIMANTS' SEAT: the constraint is a mechanism for foreclosing rights claims through burden-shifting. Historical 'evidence' is methodologically constructed and often contested. The same historical methodology produces conservative outcomes consistently, suggesting non-neutral application. Exit is impossible because constitutional rights protection is the only available remedy. FROM PROGRESSIVE LEGAL ESTABLISHMENT'S SEAT: originalism is intellectual capture—a sophisticated framework that legitimizes predetermined conservative outcomes while appearing methodologically rigorous. The constraint's power lies in institutional control (judicial appointments) defended through methodological gatekeeping. FROM HISTORIAN/ARCHIVAL EXPERTS' SEAT: originalism creates institutional demand and funded positions. But it also instrumentalizes historical research: historians are pressured to find meaning that supports originalist conclusions, and the political stakes distort scholarly objectivity. The constraint extracts intellectual labor while claiming neutrality.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges (institutional, analytical exit) sit near d=0.15: they benefit from institutional authority and methodological legitimacy, face no personal extraction. Conservative legal establishment (organized, arbitrage exit) sits near d=0.25: they directly benefit from policy outcomes and institutional dominance; they have resources and mobility to shift if originalism failed, but currently it succeeds. Contemporary rights claimants (powerless, trapped) sit near d=0.92: they bear the full cost of burden-shifting and foreclosed claims; they cannot exit. Non-originalist judges (moderate, constrained exit) sit near d=0.68: they face institutional marginalization and constrained authority; they cannot exit the judiciary without career loss. Historical evidence practitioners (organized, mobile exit) sit near d=0.35: they benefit from institutional demand but are somewhat dependent on originalism's political success; they could shift to other scholarly domains if demand collapsed. The constraint produces high directionality divergence across seats: benefits concentrate on conservative establishment and originalist judges (low d); costs disperse across rights claimants and alternative interpreters (high d). This asymmetry is the signature of tangled rope: there is a genuine coordination function (interpretive methodology constraining discretion), but it is asymmetrically captured for extraction (suppressing non-conservative rights claims).
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism's founding problem was Warren Court activism. By 2010, the founding problem had substantially resolved: the Court's composition had shifted, originalist jurisprudence was dominant, and results-oriented 1960s-style rights expansion had ended. The founding problem is now dead or contested—courts no longer need constraint against expansive rights jurisprudence. Yet the constraint persists with increasing extractiveness and suppression. This is the signature of mandatrophy (mandate outliving its function): the doctrine is maintained not because it solves the founding problem, but because it produces conservative policy outcomes and institutional dominance for the conservative legal establishment. The theater ratio rise (projected 0.18 in 1980, observed 0.42 in 2025) indicates growing gap between stated function (constraining judges) and actual maintenance (constraining contemporary rights claims). The suppression intensification (projected 0.52 in 1980, observed 0.79 in 2025) tracks the increasing difficulty of defending adaptive interpretation as originalism consolidates institutional power. By 2025, originalism's primary function is not solving the founding problem but maintaining conservative institutional advantage—a textbook mandate atrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_construction,
    'How much of ''original public meaning'' is objectively recoverable from historical evidence, versus constructed through interpretive choices by originalist scholars and judges?',
    'Historiographical meta-analysis examining cases where originalists reach different conclusions from identical historical sources; comparison of originalist and non-originalist historians'' reconstruction of ratification intent on contested clauses (Second Amendment, Commerce Clause, Equal Protection).',
    'If much of ''original meaning'' is constructed through methodological choices, the doctrine''s neutrality claim collapses and extractiveness increases substantially (judges are discretionary despite appearing bound). If ''original meaning'' is mostly objectively recoverable, the constraint''s coordination function is genuine and extractiveness is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_construction, empirical, 'Whether original public meaning is objectively historical or methodologically constructed').

omega_variable(
    methodology_vs_outcomes,
    'To what extent does originalist methodology predictably produce conservative outcomes independent of historical evidence?',
    'Statistical analysis of originalist opinions: do outcomes correlate with appointee ideology? Do originalist judges reach different conclusions on identical historical questions depending on political affiliation? Do the same historical sources yield different conclusions across ideologically diverse originalists?',
    'If originalist outcomes are ideology-predictive, the constraint is substantially extractive (masked discretion). If originalist outcomes diverge from appointer ideology, the constraint has genuine coordination power (methodology is constraining).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodology_vs_outcomes, empirical, 'Whether originalism constrains outcomes or correlates with conservative results').

omega_variable(
    foundational_axiom_vs_sibling_readings,
    'How does the originalist axiom ''constitutional meaning is fixed at ratification'' relate logically to the living constitutionalist axiom ''constitutional principles evolve with society''?',
    'Philosophical analysis of whether the two axioms foreclose each other or coexist in different frameworks. Can a single party hold both (e.g., an originalist who acknowledges principle evolution within the bounds of fixed meaning)?',
    'If they logically foreclose each other (both cannot be true in the same framework), the relation is ''forecloses'' and the readings cannot coexist in one adjudicatory system. If they can coexist (judges can apply originalism in some domains and adaptation in others), the relation is ''coexists_with'' and mixed methodologies are structurally possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_axiom_vs_sibling_readings, conceptual, 'Whether originalism and living constitutionalism are logically incompatible or can coexist').

omega_variable(
    institutional_capture_and_suppression,
    'Is the rising suppression of non-originalist interpretation (1980-2025) a feature of originalism''s logical rigor, or a side effect of conservative institutional consolidation that could be reversed by political change?',
    'Historical counterfactual: if a living constitutionalist majority gained the Supreme Court (via appointee retirements and Democratic presidency), would suppression of originalism occur symmetrically, or would the Court move toward mixed methodology allowing both approaches?',
    'If suppression is symmetric and reversible, it is an artifact of institutional dominance, not inherent to originalism. If living constitutionalism would suppress originalism equally, suppression is a property of whoever holds institutional power (structural political fact). If only originalists suppress, originalism carries unique suppressive force (methodological property).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_and_suppression, preference, 'Whether suppression of non-originalist interpretation is inherent to originalism or contingent on institutional power').

omega_variable(
    reading_identity_under_doctrine_mutation,
    'If originalist doctrine mutates substantially—e.g., originalists shift from text + historical practice to pure textualism (Scalia→Kagan textualism drift), or abandon originalism for originalist-sounding alternatives—does the originalist reading remain coherent, or has it dissolved into a different reading?',
    'Doctrinal genealogy tracking whether contemporary ''originalism'' matches Bork/Scalia foundational axioms or has been revised beyond recognition. Assessment of whether practitioners still claim recovered original meaning or have abandoned that premise while retaining the label.',
    'If originalism has substantially mutated, the current constraint may no longer be the originalist reading described here—it may be a new reading altogether, and this story''s ε and classification would shift. If the axioms persist despite surface mutations, the reading identity is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_under_doctrine_mutation, conceptual, 'Whether originalism as doctrine has maintained identity or fundamentally mutated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_text__originalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_text__originalist_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_text__originalist_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_text__originalist_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_text__originalist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_text__originalist_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_text__originalist_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_text__originalist_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_text__originalist_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_text__originalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_text__originalist_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_text__originalist_reading, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_text__originalist_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_text__originalist_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_text__originalist_reading, suppression_requirement, 2025, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, constitutional_amendment_barrier).

% DUAL FORMULATION NOTE:
% Originalism is one reading of the contested kernel 'us_constitution_text'. It is structurally distinct from and affects two sibling readings (living constitutionalism, positivism) by controlling institutional interpretation of the Constitution. Each reading produces a different constraint story with different ε values, beneficiary/victim structures, and classifications. The three stories together form a kernel family linked by their common referent (the Constitution) and structural relationships (reading_relations). See 'us_constitution_text__living_constitutionalist_reading' and 'us_constitution_text__positivist_reading' for sibling constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
