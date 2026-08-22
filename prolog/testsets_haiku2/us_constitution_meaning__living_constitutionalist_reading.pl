% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Constitutional Meaning Under Living Constitutionalism
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   This constraint captures the living constitutionalist reading of U.S.
 *   constitutional authority: the view that the Constitution's enduring
 *   principles remain stable and binding, but their application evolves as
 *   social attitudes, empirical circumstances, and moral understanding
 *   advance. Judges are constrained by constitutional text, precedent, and
 *   the principle of fidelity to enduring values, but empowered to recognize
 *   how those principles apply to unanticipated situations and contemporary
 *   contexts. This reading is one of three contested interpretations of the
 *   same constitutional kernel (alongside originalism and legal positivism);
 *   it is NOT a claim that the Constitution is infinitely malleable or that
 *   any meaning is permissible. Rather, it is the claim that the principles
 *   embedded in the Constitution (liberty, equality, due process) can
 *   accommodate rights and regulatory powers not foreseen at ratification.
 *   The extraction measured here is the transfer of interpretive authority
 *   from fixed historical meaning to contemporary judicial discretion, and
 *   the asymmetric benefit accrues to rights claimants in evolving social
 *   contexts while stable-interest holders bear the cost of predictability
 *   loss.
 *
 * KEY AGENTS:
 *   - progressive_rights_claimants: beneficiary; constrained exit; gain recognition of unenumerated rights
 *   - supreme_court_majority: agenda-setter; institutional power; sets the methodology and doctrinal frames
 *   - originalist_and_textualist_dissenters: payer; institutional power; absorb doctrinal defeats and legitimacy losses
 *   - established_property_and_contract_interests: payer; powerful but more mobile exit; bear regulatory reinterpretation
 *   - democratic_legislatures: beneficiary and payer; gain adaptive capacity, lose predictability
 *   - constitutional_scholars_and_judges: observer; analytical distance; participate in methodology contestation
 *   - conservative_originalist_coalition: excluded; would frame living constitutionalism as judicial overreach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Constitutional Meaning Under Living Constitutionalism").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '81c79a85-88d3-4e63-862a-4026810d8557').
narrative_ontology:cs_kernel_codification('81c79a85-88d3-4e63-862a-4026810d8557', fixed_text).
narrative_ontology:cs_authority_grounding('81c79a85-88d3-4e63-862a-4026810d8557', lineage).
narrative_ontology:cs_interpretation_layer_present('81c79a85-88d3-4e63-862a-4026810d8557').
narrative_ontology:cs_reading_relation('81c79a85-88d3-4e63-862a-4026810d8557', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('81c79a85-88d3-4e63-862a-4026810d8557', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('81c79a85-88d3-4e63-862a-4026810d8557', foundational, enduring_principles_transcend_ratification).
narrative_ontology:cs_axiom_status(enduring_principles_transcend_ratification, holdable).
narrative_ontology:cs_axiom_grounding('81c79a85-88d3-4e63-862a-4026810d8557', enduring_principles_transcend_ratification, deontological).
narrative_ontology:cs_axiom('81c79a85-88d3-4e63-862a-4026810d8557', foundational, judicial_adaptation_compatible_with_fidelity).
narrative_ontology:cs_axiom_status(judicial_adaptation_compatible_with_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('81c79a85-88d3-4e63-862a-4026810d8557', judicial_adaptation_compatible_with_fidelity, instrumental).
narrative_ontology:cs_reference_frame('81c79a85-88d3-4e63-862a-4026810d8557', adaptive_principle_fidelity).
narrative_ontology:cs_drift_state('81c79a85-88d3-4e63-862a-4026810d8557', contemporary, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('81c79a85-88d3-4e63-862a-4026810d8557', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, democratic_adapters).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_constraint_bearers).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, stable_property_and_contract_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, progressive_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, democratic_legislatures).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, originalist_and_textualist_dissenters).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, established_property_and_contract_interests).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, democratic_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek constitutional recognition of rights not explicitly enumerated at ratification. Under living constitutionalism, enduring principles (liberty, equal protection, due process) can accommodate their claims through evolved application to unanticipated situations. They argue the Constitution's text remains stable while its meaning responds to social change. Their exit is constrained: they cannot abandon the constitutional system and must accept whatever interpretation the courts announce.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, progressive_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Interprets the Constitution and announces binding doctrine. Under living constitutionalism, judges are empowered to identify enduring principles and adapt them to contemporary contexts, constrained by text, precedent, and principle but not by original historical meaning. The Court sets the agenda for which rights are recognized, which doctrinal tests apply, and which interpretive methodology governs future decisions. Enforcement of the constraint depends on the Court's continued willingness to entertain evolved applications and on lower courts' compliance.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, supreme_court_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Oppose living constitutionalism and argue for fixed meaning at ratification. They absorb the cost in doctrinal defeats when a living constitutionalist majority overrules their objections or takes the Constitution in directions they believe unfaithful to the text. They also pay institutional legitimacy costs if public confidence in the Court erodes due to perceived fluidity. Exit options are limited: they can dissent, write scholarly critiques, or advocate for institutional reform, but cannot opt out of the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_and_textualist_dissenters, payer,
    institutional, generational, constrained, national).

% Depend on doctrinal stability and predictability. Living constitutionalism's openness to rights expansion creates uncertainty about regulatory scope, takings doctrine, and commercial freedom. They pay through regulatory reinterpretation justified by evolved application of due process or equal protection. Their exit is more available than rights claimants': they can restructure contractual arrangements, relocate operations, or seek federalism-based limitations, but remain ultimately subject to the Court's announced constitutional reading.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, established_property_and_contract_interests, payer,
    powerful, generational, mobile, national).

% Gain adaptive capacity under evolved constitutional readings: laws reflecting contemporary values can expect judicial validation under evolved interpretations of due process or equal protection. They also pay a cost in doctrinal unpredictability: future sessions cannot assume which legislative choices will survive constitutional review, and the Court may strike down legislation on grounds unavailable at ratification. Exit options include constitutional amendment (difficult), institutional reform (Court-packing, jurisdiction-stripping), or strategic legislative design, but substantive exit is constrained.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, democratic_legislatures, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, democratic_legislatures, payer).

% Analyze and critique the constraint, articulate doctrinal implications, and influence the interpretive methodology. They occupy the analytical distance of scholars and lower-court judges who can engage in the meta-conversation about whether living constitutionalism is coherent with rule-of-law commitments without bearing primary responsibility for the Court's decisions.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, constitutional_scholars_and_judges, observer,
    institutional, generational, analytical, national).

% Would argue that living constitutionalism is unconstrained judicial power and threatens the rule of law. They are partly included as minority judges and scholars but lack agenda-setting power to determine the Court's methodology. Their exclusion is structural: under living constitutionalism's dominance, originalist objections to the methodology itself are sidelined as one interpretive view rather than treated as fundamental truth claims about constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, conservative_originalist_coalition, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, supreme_court_majority).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional authority across generations by enabling judges to interpret enduring principles as applicable to unanticipated circumstances, allowing the Constitution to remain governing law without constant amendment while maintaining fidelity to foundational values.
% TRANSFER_FUNCTION: Transfers interpretive authority from fixed historical meaning to contemporary judicial discretion. Progressive rights claimants gain recognition of unarticulated rights; the judiciary gains agenda-setting power over constitutional meaning; originalists and stable-interest holders lose the protection of fixed doctrine and institutional predictability.
% ABSENT_VOICES: The founding generation itself cannot speak; originalist advocates argue they represent the founders' intent but lack the founders' direct voice. Positivist critics who wish legal authority derived from formal enactment rather than enduring principle are partly excluded. International constitutional courts and non-U.S. legal traditions offer alternative interpretive models but remain marginalized in American doctrine.
% DISAPPEARANCE_RATIONALE: If living constitutionalism vanished and originalist fixed meaning prevailed, decades of recognized rights (privacy, marriage equality, expanded equal protection) would lose constitutional grounding and revert to statutory protection. The regulatory scope of the federal government under evolved Commerce Clause doctrine would contract. Constitutional litigation strategy and legislative calculus would reorganize around historical public meaning at ratification. The constitutional system would shift from adaptive principle-following to historical literalism.
% FOUNDING_PROBLEM: The Constitution, drafted in 1787, faced early criticism as an eighteenth-century document applied to nineteenth-century problems. How could a fixed text govern an evolving society without constant amendment? Living constitutionalism emerged to resolve this institutional adaptation problem: by allowing judges to identify enduring principles and apply them to new circumstances, the Constitution could remain governing law across generations while maintaining fidelity to foundational values.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalists and progressive legal scholars attest the founding problem is live: societies change faster than amendment procedures, and a locked-to-1787 Constitution would become obsolete. Originalists and conservative theorists, from outside the benefiting parties, attest the founding problem is misdiagnosed: the real problem is distinguishing legitimate interpretation from illegitimate judicial legislation. Comparative constitutional law shows mixed evidence: Canada and Germany amend frequently; the UK and Australia use living interpretation; no universal answer. No corroboration from outside the dispute itself: scholars and jurists within the U.S. legal community are the only voices, and they are split.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.58 at interval end) reflects the transfer of interpretive power: living constitutionalism grants judges discretion to recognize new rights and regulatory scopes not available under fixed historical meaning. This is extraction in the sense that stable-interest holders lose the protection of fixed doctrine and originalists lose the agenda-setting power to determine interpretation methodology. Suppression (0.42) is moderate because the constraint's persistence does not rely primarily on coercing compliance—originalists dissent openly, scholars debate fiercely, and the lower courts sometimes resist or reinterpret. The suppression that exists is structural: the Court's institutional power to set doctrine, the prestige of the living constitutionalist interpretive tradition in elite law schools, and the difficulty for originalists to overturn established precedents. Theater ratio (0.31) is modest because the constraint has genuine institutional function: the Court does constrain itself through doctrinal reasoning, precedent, and the limiting principle of 'enduring values.' The theater that is present appears when the Court retroactively vindicates unarticulated rights, offering originalist-styled reasoning that some critics read as post-hoc justification. The measurement trajectory shows a modest rise through year 30 (as living constitutionalism's precedents accumulate and its doctrinal reach expands), peaks around year 45 (maximum expansion of recognized rights and regulatory authority), then slightly declines by year 60 (as originalist justices add to the bench and doctrinal conflicts emerge—projected). The time grid is uniform: all three metrics are authored at every time point (0, 10, 20, 30, 45, 60), enabling temporal analysis to detect extraction accumulation, theater creep, and enforcement intensity changes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Supreme Court majority) experiences this constraint as a framework enabling constitutional governance: principles guide discretion, precedent constrains arbitrariness, and adaptation serves the Constitution's purpose. The beneficiary seats (rights claimants) experience it as liberation: unarticulated rights gain recognition because enduring principles can encompass evolved understandings. The payer seats experience it oppositely: originalists see unconstrained judicial power; stable-interest holders see regulatory unpredictability; conservative judges experience it as institutional loss (minority voting power). The constraint's type classification should diverge by seat: an agenda-setter analyzing from the Court's perspective might compute this as rope (genuine coordination of constitutional governance); a rights claimant from a marginalized group might compute it as snare (the 'evolved understanding' may never reach their situation); an originalist payer would compute it as snare (coercive reinterpretation). The engine computes per-seat using directionality derived from power, exit options, and beneficiary/victim membership, so this divergence emerges from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive rights claimants sit near the beneficiary end of directionality (d ≈ 0.1–0.2): the constraint's operation directly benefits them, and their exit options are constrained (they cannot opt out of constitutional governance). The Supreme Court majority sits near neutral or slightly beneficiary (d ≈ 0.3–0.4): they set the agenda and exercise interpretive power, but they are also bound by the constraint's limiting principles (they cannot simply invent any meaning). Originalist dissenters sit near the payer end (d ≈ 0.7–0.8): they lose doctrinal defeats repeatedly, and their ability to shift the Court's methodology is constrained by majoritarian voting and institutional inertia. Stable-interest holders sit near the payer end (d ≈ 0.65–0.75): they bear regulatory unpredictability and cannot easily exit (property is territorial, contracts are locked in). Democratic legislatures sit near symmetric (d ≈ 0.5): they gain adaptive capacity but also lose doctrinal predictability, and their exit options are moderately constrained (they can amend but require supermajority consensus). The directionality profile is non-uniform across seats, which is the point: the constraint extracts differently depending on the agent's structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   Living constitutionalism does NOT exhibit mandatrophy in the classical sense (founding problem dead, arrangement persists by inertia). The founding problem—how to interpret a fixed text in evolving circumstances—remains live. However, there is a risk of secondary mandatrophy: if the constraint's actual operation drifts from adaptive principle-following toward theater (judges reasoning backward from policy preferences), the founding problem could become decoupled from the arrangement's actual function. The measurement trajectory includes this risk: theater_ratio rises from 0.18 to 0.31, suggesting some increase in retroactive reasoning or confirmation of prior outcomes. If theater continues to rise and extractiveness plateaus, mandatrophy would be indicated. The constraint is NOT mandatrophic at the interval's current endpoint, but the trajectory warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enduring_principle_vs_arbitrary_invention,
    'What distinguishes identifying enduring constitutional principles from post-hoc invention of new rights? Where is the boundary between legitimate adaptation and illegitimate judicial legislation?',
    'Comparative analysis of cases where the Court identified claimed principles and subsequent doctrinal acceptance/rejection; examination of consistency in how principles are articulated and applied across decisions; philosophical and jurisprudential study of the principle-identification methodology.',
    'If the boundary can be clearly articulated and enforced, living constitutionalism retains constraint and rule-of-law grounding. If the boundary is systematically crossed (cases show post-hoc principle-invention), the constraint reclassifies toward snare (unconstrained extraction dressed as interpretation). If the boundary cannot be articulated at all, the reading may be conceptually incoherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enduring_principle_vs_arbitrary_invention, conceptual, 'Whether enduring principles can be identified with constraint or whether principle-identification is fundamentally post-hoc.').

omega_variable(
    contemporary_consensus_standard,
    'How is contemporary moral consensus identified and weighted in constitutional interpretation? Whose consensus counts (legal scholars, judges, the public, affected minorities)? What happens when consensus is contested or evolving?',
    'Empirical analysis of how judges invoke consensus (citation patterns, sources cited); comparative study of different methodologies for identifying consensus; historical analysis of cases where invoked consensus was later repudiated or proven false.',
    'A clear, transparent consensus-identification methodology would constrain living constitutionalism and reduce theater (judges would be accountable to stated standards). Vague or judge-selected consensus standards would increase extractiveness (judicial discretion widens) and theater (retroactive consensus claims). Disagreement over whose consensus matters (majority vs. affected minority) reveals the reading''s vulnerability to majoritarian capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_consensus_standard, empirical, 'Whether contemporary moral consensus is a determinable fact or a cover story for judicial preference.').

omega_variable(
    reading_coherence_vs_originalist_critique,
    'Is living constitutionalism internally coherent? Can enduring principles be stable if their application varies with every generation''s circumstances? How is this different from saying the Constitution means whatever judges say it means?',
    'Jurisprudential analysis of living constitutionalism''s logical structure; examination of whether the reading''s core premise (stable principles + evolving application) survives critical scrutiny; comparative analysis with originalism and positivism to isolate the unique vulnerabilities.',
    'If living constitutionalism is shown to be internally coherent and defensible against the originalist critique, the reading retains legitimacy and the constraint''s extraction is interpreted as justified judicial discretion. If the critique is upheld, the reading may be reclassified as theater masking unconstrained extraction (closer to snare). If coherence depends on empirical contingencies (e.g., judges'' epistemic access to enduring principles), the reading''s stability becomes contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coherence_vs_originalist_critique, conceptual, 'Whether the core premise of living constitutionalism withstands logical scrutiny.').

omega_variable(
    court_composition_and_reading_stability,
    'How stable is the living constitutionalist reading as the dominant judicial methodology? What happens if originalist justices gain majority status?',
    'Tracking of Court composition and doctrinal shifts; analysis of cases decided by originalist majorities vs. living constitutionalist majorities; projection of future Court ideological balance and likely doctrinal direction.',
    'High instability in the reading (vulnerable to Court composition shifts) would indicate the reading''s authority rests on contingent institutional power rather than principled consensus. If originalists gain a stable majority, living constitutionalism may be formally repudiated, and the constraint would reorganize entirely. The constraint''s extractiveness and type would shift based on who holds the Court majority—the reading''s meaning would become read-dependent and institutionally volatile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(court_composition_and_reading_stability, empirical, 'Whether living constitutionalism depends on current Court ideology or reflects deeper jurisprudential consensus.').

omega_variable(
    foundational_axiom_conflict,
    'Do the foundational axioms of living constitutionalism and originalism genuinely foreclose each other, or do they merely describe different interpretive priorities that a legal system could hold simultaneously at different levels?',
    'Detailed analysis of the logical structure of each axiom; examination of whether a mixed system (some doctrines originalist, some living constitutionalist) is incoherent or merely pragmatic; study of comparative jurisdictions that employ mixed methodologies.',
    'If the axioms foreclose each other, the reading_relations should include ''forecloses'' links to siblings. If they merely describe competing priorities, ''coexists_with'' is the correct relation. This determines whether the three readings can coexist in a single system or whether one must prevail. A mixed system would indicate the readings influence but do not foreclose each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_axiom_conflict, conceptual, 'Whether living constitutionalism and originalism describe incommensurable interpretive frameworks or pragmatic priorities that can coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(us_c_be_t45, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(us_c_su_t45, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, judicial_review_and_legislative_supremacy).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, rights_expansion_vs_property_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel us_constitution_meaning. Sibling readings (originalist_reading, positivist_reading) instantiate the same kernel with different authority groundings and axioms. All three readings are linked via reading_relations in the cs_structure. The three readings together form a constraint family; cross-reading comparison reveals how judicial interpretive methodology drives divergent extraction profiles and type classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__living_constitutionalist_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
