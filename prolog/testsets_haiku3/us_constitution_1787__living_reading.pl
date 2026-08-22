% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: US Constitution (1787) — Living Constitutionalism Reading
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The living constitutionalism reading treats the 1787 text as an
 *   aspirational framework whose meaning evolves with contemporary
 *   understandings of rights and justice. Rather than locking meaning to the
 *   framers' intent or constraining courts to textual amendment, this reading
 *   permits judges to interpret foundational principles (liberty, equality,
 *   due process) to accommodate modern rights claims (privacy, dignity,
 *   reproductive autonomy, sexual orientation equality) that the 1787 text
 *   does not enumerate. The reading is one instantiation of a contested
 *   kernel: the US Constitution itself. The originalist reading (meaning
 *   fixed at ratification) and the positivist reading (meaning constrained to
 *   text + formal amendment) are alternative readings of the same kernel
 *   text, authored as separate constraint stories linked via
 *   network.affects_constraints. This story generates the living reading
 *   alone: it names the beneficiaries (progressive judicial coalitions,
 *   rights movements), victims (textual constraint advocates, federalists),
 *   and describes how the constraint operates from the living reading's own
 *   interpretive standpoint. The claim/metric gap is deliberate: the reading
 *   CLAIMS coordination (rights adaptation), but the metrics describe
 *   substantial extraction (judicial discretion decoupled from textual
 *   constraint, suppression of originalist alternatives) and rising theater
 *   (judges increasingly frame policy outcomes as constitutional discovery).
 *   The engine detects this divergence; the story does not reconcile it.
 *
 * KEY AGENTS:
 *   - Progressive judicial coalitions — institutional agenda-setters; control the meaning-making process; benefit from interpretive discretion
 *   - Rights advocacy movements — organized beneficiaries; gain expanded protections without formal amendment; constrained by reliance on judicial favor
 *   - Originalists and textual constraint advocates — payers; experience their interpretive framework as marginalized and defeated; trapped exit (require judicial appointments or constitutional amendment)
 *   - Federalism defenders — institutional payers; lose regulatory autonomy as federal constitutional meaning expands; constrained by amendment friction
 *   - Judicial institutions — agenda-setters; gain interpretive authority; must actively enforce/defend the reading against originalist challenge
 *   - Democratic majorities — organized beneficiaries; can influence constitutional meaning through appointments and mobilization without amendment; diffuse costs from reversibility
 *   - Originalist coalition — excluded; structured out of the living reading's framework as defeated rather than live alternative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.68).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.71).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "US Constitution (1787) — Living Constitutionalism Reading").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '11310125-c373-495f-912e-82be235d5aa5').
narrative_ontology:cs_kernel_codification('11310125-c373-495f-912e-82be235d5aa5', fixed_text).
narrative_ontology:cs_authority_grounding('11310125-c373-495f-912e-82be235d5aa5', extraction).
narrative_ontology:cs_interpretation_layer_present('11310125-c373-495f-912e-82be235d5aa5').
narrative_ontology:cs_reading_relation('11310125-c373-495f-912e-82be235d5aa5', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('11310125-c373-495f-912e-82be235d5aa5', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('11310125-c373-495f-912e-82be235d5aa5', foundational, constitutional_meaning_normatively_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_normatively_evolves, holdable).
narrative_ontology:cs_axiom_grounding('11310125-c373-495f-912e-82be235d5aa5', constitutional_meaning_normatively_evolves, deontological).
narrative_ontology:cs_axiom('11310125-c373-495f-912e-82be235d5aa5', secondary, interpretation_binds_evolving_consensus).
narrative_ontology:cs_axiom_status(interpretation_binds_evolving_consensus, holdable).
narrative_ontology:cs_axiom_grounding('11310125-c373-495f-912e-82be235d5aa5', interpretation_binds_evolving_consensus, conventional).
narrative_ontology:cs_reference_frame('11310125-c373-495f-912e-82be235d5aa5', framers_aspirational_intent_framework).
narrative_ontology:cs_drift_state('11310125-c373-495f-912e-82be235d5aa5', contemporary_post_dobbs_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('11310125-c373-495f-912e-82be235d5aa5', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, progressive_judicial_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, rights_advocacy_movements).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, constitutional_text_literalists).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, federalism_defenders).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, textual_constraint_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, democratic_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, regulatory_authorities).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, judicial_norm_responsiveness).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, rights_expansion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judicial majorities adopting living constitutionalism interpret the text's aspirational language to reach outcomes aligned with contemporary rights claims (privacy, dignity, equality). They control the agenda by deciding which 'evolving norms' count as binding and frame judicial power as responsive to moral progress rather than locked in the framers' intent. They set precedent and define the scope of constitutional protections.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, progressive_judicial_coalitions, agenda_setter,
    institutional, generational, arbitrage, national).

% Civil rights, LGBTQ+, reproductive justice, and other rights movements use the living reading to argue for constitutional protection of modern claims not enumerated in the 1787 text (privacy rights, equal dignity, etc.). They benefit from judicial expansion without needing to amend the text, gaining substantial rights gains that amendment would not deliver. Their leverage is cultural/political mobilization, which influences but does not determine judicial interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, rights_advocacy_movements, beneficiary,
    organized, generational, constrained, national).

% Originalists, constitutionalists who favor fixed meaning, and those who believe the amendment process is the legitimate path to constitutional change bear the cost of having their interpretive framework marginalized when courts adopt living readings. They argue the constraint removes the text's binding force and treats the constitution as clay for judicial reshaping. Their exit is legislative amendment or appointment of originalist justices — both high-friction, long-horizon, require sustained political power.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, textual_constraint_advocates, payer,
    powerful, biographical, constrained, national).

% State legislatures and federalist advocates lose regulatory authority when the living reading expands federal constitutional protections (e.g., due process, equal protection interpreted to reach conduct the 1787 framers left to state discretion). They pay through preemption of state law and lost policy space. Their recourse is state constitutional amendment or federal constitutional amendment — both require sustained coalition and are slow.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federalism_defenders, payer,
    institutional, generational, constrained, national).

% Legal scholars and jurists committed to textual fidelity and fixed meaning experience the living reading as epistemically lawless — it permits judges to reach any outcome by invoking 'evolving norms' without constraint. They are structurally trapped: their interpretive framework is dismissed as anachronistic, they have no veto over judicial appointments or doctrine, and the legitimacy of their objection is itself contested by the living reading (which treats them as resisting moral progress).
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_text_literalists, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, constitutional_text_literalists, excluded).

% Courts adopt and enforce living constitutionalism as a framework. They gain interpretive discretion, ability to reach equity outcomes, and insulation from textual constraint. The constraint requires active enforcement: courts must repeatedly re-articulate which evolving norms bind constitutional meaning, defend against originalist challenges, and distinguish permissible evolution from judicial fiat. Judicial power to define the constraint is also the power to absorb social pressure.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, judicial_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Citizens mobilizing around contemporary rights and policy preferences can influence constitutional meaning through cultural pressure, appointments, and social movements without formal amendment. Democratic majorities gain access to constitutional change that bypasses the high friction of Article V amendment. They also carry diffuse costs: judicial discretion means rights gains can be reversed, and the constraint's legitimacy depends on perceived alignment with genuine social consensus rather than elite preference.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_majorities, beneficiary,
    organized, biographical, mobile, national).

% The originalist coalition — conservative judges, scholars, federalist organizations — is structurally excluded from the living reading's framework. Their core premise (meaning fixed at ratification) is treated as a defeated position rather than a legitimate alternative. They mount sustained resistance through appointments and dissenting opinions but cannot override the living reading's authority from within the institutional structure.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_reading_advocates, excluded,
    powerful, generational, trapped, national).

% Executive and administrative agencies face unpredictable constitutional boundaries when courts reinterpret rights and liberties under living constitutionalism. Policies initially thought constitutional may be struck down as evolving norms shift. They pay through policy reversals, litigation costs, and reduced regulatory autonomy. Their recourse is appointment influence and legislative statutes — both constrained by judicial review.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, regulatory_authorities, payer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, progressive_judicial_coalitions).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for constitutional meaning to adapt to contemporary understandings of rights and justice without requiring formal amendment; coordinates the legal system's response to social change and moral consensus; enables courts to resolve constitutional questions about conduct and rights the 1787 framers could not have anticipated.
% TRANSFER_FUNCTION: Transfers interpretive authority from text-bound historical inquiry to contemporary judicial judgment about 'evolving norms'; moves authority to decide constitutional meaning from amendment process (high friction, democratic veto points) to appellate courts (concentrated, minoritarian, responsive to legal culture elite). Rights advocacy movements gain expanded protections; textual and federalist constraint advocates lose binding textual authority.
% ABSENT_VOICES: Originalists and positivists who believe meaning is fixed or constrained by amendment process are structurally excluded — their interpretive framework is marginalized within the living reading paradigm as a defeated rather than live alternative. The 1787 framers themselves are absent (their intent is treated as historically interesting but not binding). State regulatory majorities who prefer stable, predictable boundaries are not at the judicial table.
% DISAPPEARANCE_RATIONALE: If living constitutionalism disappeared and courts reverted to originalism or positivism, constitutional meaning would lock to 1787 framers' intent or democratic amendment; contemporary rights claims (privacy, dignity, sexual orientation equality) without explicit constitutional text would lose judicial protection; state regulatory autonomy would expand dramatically; the constitutional space available to rights advocacy movements would shrink to amendment-only paths with high friction. The legal and political system would reorganize around different sources of authority.
% FOUNDING_PROBLEM: The 1787 constitutional text anticipated a stable legal order for 18th-century governance but could not enumerate every right or predict how social understandings of liberty, equality, and personhood would evolve. A rigid reading locks constitutional meaning to historical context; some mechanism must accommodate rights claims arising from changed social conditions without requiring formal amendment every generation.
% FOUNDING_PROBLEM_CORROBORATION: Progressive legal scholars and rights advocates affirm the founding problem is live and living constitutionalism solves it. Originalists and conservatives contest both the problem (arguing framers used general language precisely to allow amendment) and the solution (arguing living constitutionalism is judicial fiat, not legitimate interpretation). Independent constitutional law scholarship splits: modernists acknowledge social change creates pressure; originalists counter that pressure should route through Article V, not judicial discretion. No unanimous corroboration from outside the benefiting coalition.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects judicial discretion to reach outcomes by invoking 'evolving norms' without anchoring to historical text; the interpretation is decoupled from marginal constraint cost. Suppression at 0.71 reflects the active judicial enforcement required to maintain living constitutionalism against originalist doctrinal alternatives and to exclude those alternatives' legitimacy (treating them as anachronistic rather than equally viable). Theater at 0.48 reflects judicial framing of policy outcomes as moral discovery and constitutional fidelity when the operative mechanism is judicial choice about which norms count as binding. Accessibility collapse at 0.62 reflects that alternatives (originalism, amendment-only paths) remain epistemically and institutionally available to opponents but are marginalized by the living reading's institutional dominance. Resistance at 0.74 reflects sustained originalist and federalist push-back through dissents, appointments, scholarship, and state-level counter-mobilization. The measurement series tracks living constitutionalism's strengthening from 1960 (nascent, Warren Court era) through 2025 (consolidated, but under sustained attack): extractiveness rises as the constraint accumulates more rights extensions; theater rises as the constraint must expend more institutional effort maintaining legitimacy against challenge; suppression rises as originalist alternatives require active delegitimization rather than passive displacement. All metrics share one time grid (1960, 1975, 1990, 2005, 2015, 2025) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive judicial coalition seat, living constitutionalism is genuine moral progress and responsive adaptation to social change — the constraint coordinates rights protection across changing circumstances. From the originalist seat, the same structure is judicial fiat and violation of textual constraint — extractive power veiled as constitutional interpretation. From the federalist seat, it is preemption of state regulatory autonomy under the cover of evolving norms. From the rights advocacy seat, it is the only politically feasible path to protection of contemporary rights claims. The engine computes this divergence as different d values per seat: progressive judges sit near the beneficiary end (d near 0.0), originalists sit near the target end (d near 1.0), federalists sit intermediate. The seated classification divergence follows from the structural data (beneficiary/victim declarations + power/exit modulation), not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive judicial coalitions (institutional, arbitrage exit) are the structural beneficiaries: they collect interpretive authority and control the meaning-making agenda without needing to amend the text. Extracted value flows to them as institutional power. Originalists and textual constraint advocates (powerful and moderate power, constrained exit) are the structural payers: they experience their interpretive framework as marginalized and delegitimized; their exit is blocked by institutional lock-in (require sustained judicial appointments or constitutional amendment, both high friction). Federalism defenders (institutional, constrained exit) are also payers: they lose regulatory space as federal constitutional protections expand. Rights advocacy movements and democratic majorities are beneficiaries but also carry diffuse diffuse costs (reliance on judicial goodwill, reversibility of gains). The directionality derivation tracks these asymmetries from the base beneficiary/victim declarations and exit constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Living constitutionalism resolves a genuine coordination problem — how to interpret an 18th-century text to address rights questions the framers could not anticipate — without requiring formal amendment every generation. However, the solution exhibits substantial extraction asymmetry: judicial discretion to define 'evolving norms' creates concentrated gains for legal culture elites and progressive coalitions, while imposing diffuse costs on textual constraint advocates and federalists (via preemption). The theater ratio rising from 0.22 to 0.48 signals increasing performative maintenance: courts must repeatedly re-narrate policy outcomes as constitutional discovery and defend against the originalist challenge that the outcomes are judicial fiat. This rises most sharply 1960–1990 (Warren and early Burger Courts, civil rights and privacy doctrine expansion) and plateaus 2005–2025 (originalist appointments increase, limiting new expansions). The constraint persists despite the attack because institutional design — lifetime appointments, appellate jurisdiction, precedent — locks in the progressive coalition's control of meaning-making. The founding problem remains contested: beneficiaries argue evolving norms are a genuine constraint on meaning (moral progress has boundaries); originalists argue 'evolving norms' is unbounded judicial discretion. The measurement series does not resolve this; it shows that institutional momentum favors the living reading despite sustained theoretical resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_norms_definition,
    'What constitutes an ''evolving norm'' that binds constitutional meaning? Who defines it and by what process?',
    'Document actual judicial practice: which social movements successfully shift constitutional meaning; which fail; whether courts track opinion polling, elite consensus, or sustained social mobilization; whether definition rules are articulated or tacit. Compare to originalist and positivist constraint stories to measure definitional variance.',
    'If evolving norms is defined by transient elite preference or social media consensus, the constraint is high-extraction snare with elite capture. If defined by sustained democratic mobilization or empirical rights discovery, the coordination function is stronger and extraction lower. The definition determines whether the constraint is tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_norms_definition, empirical, 'The operationalization of ''evolving norms'' in judicial practice.').

omega_variable(
    reversibility_of_gains,
    'How stable are constitutional rights gains achieved through living constitutionalism when judicial composition shifts?',
    'Track doctrinal reversals post-Dobbs decision (2022 overturning Roe v. Wade); measure whether privacy, dignity, and other living-reading expansions survive originalist Court appointments; compare to rights protected by explicit constitutional text or formal amendment.',
    'If living-reading gains are readily reversed by judicial appointments (as Dobbs suggests), the constraint is a snare for rights beneficiaries who have no stable claim. If sustained, the coordination function is stronger. High reversibility shifts the constraint toward snare; low reversibility supports tangled rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_of_gains, empirical, 'Whether rights gains from living constitutionalism are durable or contingent on judicial composition.').

omega_variable(
    elite_capture_of_norm_evolution,
    'Does ''evolving norms'' track genuine democratic consensus or does it privilege legal culture elite preferences, progressive coalitions, and organized movements over diffuse majorities?',
    'Compare courts'' invoked norms to opinion polling, state legislative action, and grass-roots mobilization. Measure whether courts adopt norms opposed by majority publics or norms lacking broad social backing. Assess whether rights advocacy movements with concentrated organization and legal resources drive norm adoption more than diffuse publics.',
    'High elite capture (evolving norms = lawyer/academic/activist preferences, not democratic consensus) makes the constraint a snare for unorganized majorities paying the costs. Low capture supports the coordination framing. This determines whether beneficiary claims are genuine rights movements or organized special interests extracting constitutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_norm_evolution, empirical, 'Whether ''evolving norms'' reflects democratic consensus or concentrated elite preference.').

omega_variable(
    kernel_reading_coexistence,
    'Can living constitutionalism coexist with originalism as equally legitimate readings of the same kernel, or does one foreclose the other?',
    'Examine whether courts can acknowledge originalism as a coherent interpretive method with its own internal logic while declining to adopt it, or whether courts treat originalism as epistemically invalid (anachronistic, failed, illegitimate). Same for positivism.',
    'If coexistence is possible, the constraint is structured as a choice among live alternatives held by different coalitions — more like a tangled rope. If one reading forecloses the others, the constraint operates more like a snare with institutional victors and losers. The 2022 Dobbs decision and originalist appointments suggest coexistence may be eroding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether alternative constitutional readings can coexist as legitimate framings or whether one forecloses the others.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.71) structural (institutional barriers preventing originalist alternatives from competing in interpretation) or internalized (originalists accept defeat and internalize that their framework is anachronistic)?',
    'Track originalist scholarship, dissent production, and institutional mobilization post-Dobbs. If originalists mount sustained doctrinal challenge and conservative appointments increase, suppression is structural and reversible. If originalist challenge attenuates, suppression is partly internalized (the defeated accept the constraint). Measure resistance trajectory.',
    'Structural suppression is more fragile and reversible; internalized suppression persists even after structural constraints relax. High internalization shifts the constraint from actively-enforced tangled rope toward piton (maintained by institutional inertia and narrative. The post-2016 originalist revival suggests suppression was structural, not internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether the suppression of originalist alternatives is maintained by institutional barriers or internalized abandonment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1960, us_constitution_1787__living_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t1960, projected).
narrative_ontology:measurement(us_c_tr_t1975, us_constitution_1787__living_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t1975, observed).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_1787__living_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t1990, observed).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_1787__living_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t2005, observed).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_1787__living_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(us_c_tr_t2015, observed).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__living_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(us_c_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1960, us_constitution_1787__living_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement_basis(us_c_be_t1960, projected).
narrative_ontology:measurement(us_c_be_t1975, us_constitution_1787__living_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement_basis(us_c_be_t1975, observed).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_1787__living_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(us_c_be_t1990, observed).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_1787__living_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement_basis(us_c_be_t2005, observed).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_1787__living_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(us_c_be_t2015, observed).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__living_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(us_c_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1960, us_constitution_1787__living_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement_basis(us_c_su_t1960, projected).
narrative_ontology:measurement(us_c_su_t1975, us_constitution_1787__living_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(us_c_su_t1975, observed).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_1787__living_reading, suppression_requirement, 1990, 0.64).
narrative_ontology:measurement_basis(us_c_su_t1990, observed).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_1787__living_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(us_c_su_t2005, observed).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_1787__living_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(us_c_su_t2015, observed).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__living_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(us_c_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The US Constitution (1787) kernel is instantiated as three constraint stories: living_reading (this story), originalist_reading, and positivist_reading. Each reading represents a coherent but contested interpretation of the same constitutional text and carries its own ε, beneficiary/victim structure, and type classification. Living constitutionalism exhibits higher extraction (ε=0.68) and active suppression of alternatives than originalism's textual fixity reading. The network links show which readings influence or foreclose others: living reading coexists with originalism and positivism as live institutional alternatives held by different coalitions, but the living reading has institutional dominance (Supreme Court majority, legal academia prestige). The three readings form a constraint family linked by kernel identity, not by causal or network influence in the ordinary sense — they are alternative framings of the same governance object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__living_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
