% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_sovereignty_primacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems: Sovereignty Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the SOVEREIGNTY PRIMACY READING of the
 *   contested One Country, Two Systems kernel. Under this reading, Hong
 *   Kong's autonomy is framed as a delegated power, revocable by the PRC
 *   central authority, with national security and territorial integrity
 *   overriding local autonomy claims whenever they conflict. The 2020
 *   National Security Law and its enforcement apparatus represent the
 *   operational instantiation of this reading: mainland security officials
 *   operate in Hong Kong with extraterritorial powers, the judiciary's
 *   independence is circumscribed on national security matters, and political
 *   opposition is criminalized under vague national security categories. This
 *   reading competes with two sibling readings: (1) the AUTONOMY PRIMACY
 *   reading, which interprets One Country, Two Systems as granting Hong Kong
 *   substantive, treaty-protected autonomy with meaningful checks on mainland
 *   interference, and (2) the BALANCED COEXISTENCE reading, which frames the
 *   system as requiring negotiated accommodation between sovereignty and
 *   autonomy rather than legal supremacy of either. The constraint's
 *   extractiveness has accumulated dramatically since the 2020 National
 *   Security Law (0.35→0.81 over 28 years, with the steepest rise 2015-2025),
 *   and suppression has intensified in tandem (0.42→0.88), signaling a shift
 *   from coordination to enforced extraction.
 *
 * KEY AGENTS:
 *   - PRC Central Authority: Sets the constitutional and legal framework; interprets sovereignty scope; retains power to revoke Hong Kong autonomy. Institutional power, arbitrage exit options (can choose enforcement intensity, can negotiate terms with Hong Kong elites).
 *   - Mainland Security Apparatus: Operates the National Security Law enforcement machinery in Hong Kong, including Liaison Office jurisdiction expansion and mainland detention protocols. Institutional power, arbitrage exit (operates with immunity from Hong Kong courts).
 *   - Hong Kong Civil Society: Bears suppressive force of National Security Law and its vague categories (subversion, secession, collusion). Faces arrest, detention, self-censorship. Powerless, identity-locked exit (to leave means abandoning home, profession, social bonds).
 *   - Hong Kong Judiciary: Retains formal authority on most matters but must cede jurisdiction on national security cases to mainland-influenced authorities. Loses independence on the constraint's core function. Powerful globally, but constrained locally by political pressure and retirement threats.
 *   - Political Opposition Movements: Democratic opposition and pro-autonomy organizers face prosecution under National Security Law for speech, assembly, electoral participation. Moderate power, identity-locked exit (many have fled; those remaining face continuous prosecution risk).
 *   - Hong Kong Business Elite: Gain preferential access and relationship networks through Beijing authority; business-as-usual compliance is rewarded. Powerful, arbitrage exit (can relocate or hedge exposure internationally).
 *   - International Treaty Bodies and Governments: Contend the One Country, Two Systems promise required autonomous protections. Structurally barred from enforcement within Hong Kong; can only document and protest.
 *   - Analytical Observer Seat (this generation): Traces the constraint's evolution, documents the reading's incompatibility with the autonomy-primacy interpretation, and measures the structural shift from coordination to extraction over time.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.81).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '27f2a272-810d-4478-928d-ad03b4a090ce').
narrative_ontology:cs_kernel_codification('27f2a272-810d-4478-928d-ad03b4a090ce', fixed_text).
narrative_ontology:cs_authority_grounding('27f2a272-810d-4478-928d-ad03b4a090ce', extraction).
narrative_ontology:cs_interpretation_layer_present('27f2a272-810d-4478-928d-ad03b4a090ce').
narrative_ontology:cs_reading_relation('27f2a272-810d-4478-928d-ad03b4a090ce', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('27f2a272-810d-4478-928d-ad03b4a090ce', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('27f2a272-810d-4478-928d-ad03b4a090ce', foundational, prc_constitutional_supremacy_indivisible).
narrative_ontology:cs_axiom_status(prc_constitutional_supremacy_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('27f2a272-810d-4478-928d-ad03b4a090ce', prc_constitutional_supremacy_indivisible, deontological).
narrative_ontology:cs_axiom('27f2a272-810d-4478-928d-ad03b4a090ce', foundational, national_security_override_absolute_on_autonomy_conflict).
narrative_ontology:cs_axiom_status(national_security_override_absolute_on_autonomy_conflict, holdable).
narrative_ontology:cs_axiom_grounding('27f2a272-810d-4478-928d-ad03b4a090ce', national_security_override_absolute_on_autonomy_conflict, deontological).
narrative_ontology:cs_reference_frame('27f2a272-810d-4478-928d-ad03b4a090ce', prc_constitutional_supremacy_indivisible).
narrative_ontology:cs_drift_state('27f2a272-810d-4478-928d-ad03b4a090ce', post_national_security_law_enforcement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('27f2a272-810d-4478-928d-ad03b4a090ce', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, political_opposition_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Measured at 0.81 at interval end. The constraint transfers political authority from Hong Kong autonomous institutions to mainland security organs without proportional compensation to Hong Kong citizens (compensation flows only to business elites via preferential access). The founder of One Country, Two Systems was Deng Xiaoping; his formulation envisioned Hong Kong as a test case for integrating non-mainland territories under a two-level system. The sovereignty-primacy reading treats that two-level system as fundamentally hierarchical: the mainland level has supremacy, and the Hong Kong level is administrative delegation. This is explicitly extractive because it reserves all appeal powers and all overrides to the mainland center. Suppression: At 0.88, the National Security Law provides legal cover for suppressing pro-autonomy speech, assembly, and electoral participation. The vague categories (subversion, secession, collusion with foreign forces) allow selective enforcement against targeted opponents. The suppression is actively maintained: the Liaison Office conducts surveillance and coordination with local police; mainland authorities operate detention facilities outside Hong Kong's legal system; judges who issue unfavorable rulings face non-renewal or retirement. Theater Ratio: At 0.42, moderately elevated. The constraint maintains the appearance of a two-system arrangement (Hong Kong's Common Law system, distinct legislative process, separate administrative structure), but an increasing share of enforcement activity defends the security override rather than the autonomy function. The language of 'one country' and 'national security indivisibility' is increasingly invoked to justify removing areas from Hong Kong jurisdiction entirely. Accessibility Collapse: At 0.72 at endpoint. For individuals and organizations engaged in political opposition, the collapse is near-total (exits available: physical relocation, professional ruin, identity abandonment, or silence). For business elites and politically-neutral citizens, alternatives remain (operate within constraint, migrate selectively). At the structural level, the One Country framework itself is presented as unquestionable, making exit from the constraint itself impossible without rejecting Chinese sovereignty altogether. Resistance: At 0.78 at endpoint. Hong Kong continues to mount organized resistance (pro-democracy protests 2019-2020, pro-autonomy electoral campaigns, international appeals), but the constraint's enforcement machinery has successfully suppressed organizational coordination, driven opposition leaders into exile, and criminalized continued resistance. Resistance is high but decreasingly effective.
 *
 * PERSPECTIVAL GAP:
 *   The mainland security apparatus and PRC central authority compute the constraint as ROPE (genuine coordination between a center and a peripheral region, solving the integration problem through a two-level system). From this seat, the National Security Law is a necessary tool to prevent separatism and maintain the territorial integrity that makes the coordination possible. Hong Kong civil society and the political opposition compute the constraint as SNARE (the autonomy is illusory, the Two Systems promise is abandoned, and the suppression is pure coercion masquerading as legitimate national-security governance). From this seat, there is no coordination benefit—only extraction and control. The engine will compute different types for each seat from the structural data: the beneficiary seats (mainland authority, business elite) will compute one way; the victim seats (civil society, opposition) will compute another. The engine does not reconcile this divergence—it measures it. The authored claim (Tangled Rope) reflects the constraint's formal structure (it does coordinate integration and does solve a state-assembly problem), but the metrics reflect the asymmetry of who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   PRC Central Authority: d near 0.0 (full beneficiary). Collects authority, territorial control, and security assurance. Exit options = arbitrage (can choose enforcement intensity, can modify the framework). Power = institutional. Mainland Security Apparatus: d near 0.1 (beneficiary with operational autonomy). Gains operational capacity, exemption from oversight, and budget/authority expansion. Exit = arbitrage. Power = institutional. Hong Kong Business Elite: d ≈ 0.25 (net beneficiary despite some costs). Gain preferential access, stable market conditions, relationship networks. Bear diffuse costs (constraints on some business partners, reputational risk from association). Exit = arbitrage (can diversify internationally or negotiate exemptions). Power = powerful. Hong Kong Judiciary: d ≈ 0.55 (symmetric, moving toward target). Retain some authority and prestige on non-security matters; lose independence on core political function. Bear costs in autonomy and institutional legitimacy. Exit = constrained (judges cannot exit without abandoning their professional identity). Power = powerful. Hong Kong Civil Society: d near 1.0 (full target). Bear the suppressive force directly. No compensation, no benefit. Exit = identity-locked (requires abandoning home, profession, identity). Power = powerless. Political Opposition: d near 1.0 (full target). Face criminalization, exile, or ongoing prosecution risk. Exit = identity-locked. Power = moderate. International Community: d ≈ 0.5 (neutral; neither benefiting nor targeted, but excluded from participation). Power = institutional, but constrained by sovereignty principle.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—the death of a constraint's founding mandate—is measurable here as the gap between the founding problem and its current status. The founding problem: 'After 1997 handover, Hong Kong needed a framework to reintegrate without capital flight or triggering separatism.' This problem was LIVE through ~2015 (there was genuine uncertainty about whether the two-system arrangement would hold; capital flight was a real risk; business confidence required institutional credibility). By 2020, with the National Security Law's passage and enforcement, the constraint has shifted: the problem it now solves is NOT integration (which is assumed), but rather SUPPRESSION OF DISSENT within the integrated system. The coordination function (allowing Hong Kong markets and institutions to operate as Hong Kong) has been subordinated to the security function (ensuring mainland authority override on any claim of autonomy). This reading does not declare mandatrophy resolved because the constraint's formal claim (One Country, Two Systems) remains in effect; rather, the founding problem's status is CONTESTED. The PRC central authority maintains that separatism remains live (founding problem status = live). Hong Kong civil society and international observers contend that the problem was either solved long ago or was reframed to justify a shift from coordination to control (founding problem status = dead, but the constraint persists as a theater for extractive authority). This contest is the measurement site: if the constraint disappeared, would the world rearrange or stay the same? The answer is world_rearranges, because Hong Kong's political and judicial institutions would immediately reorganize (free elections, judicial independence restoration, international engagement resumption), confirming that the constraint's persistence is enforced, not natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_redefinition,
    'Has the constraint''s founding problem been SOLVED (separatism and capital flight are no longer risks), or has it been REDEFINED to justify enforcement that was not originally contemplated?',
    'Historical analysis of threat assessments: Compare 1997-2010 security concerns (actual separatist movements, international pressure) with post-2015 threat inflation (speech-as-subversion, pro-democracy activism as separatism). If threat categories have expanded dramatically while actual insurgent capacity has declined, the problem was redefined, not solved. Corroborate by analyzing which populations are prosecuted under the National Security Law and whether their actual activities constitute security threats by any standard other than ''dissent from mainland authority''.',
    'If the founding problem was redefined, the constraint has shifted from ROPE (coordination solving a genuine problem) to SNARE (extraction using a cover story). If it was solved and new problems emerged, the constraint may have legitimately evolved. This difference determines whether the constraint''s persistence is coordination maintenance or rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_redefinition, empirical, 'Whether the national-security threat landscape has changed or been reframed to justify expanded override.').

omega_variable(
    autonomy_substantivity_question,
    'Can One Country, Two Systems be GENUINELY TWO-SYSTEMS while national security authority rests entirely with the mainland center, or is the two-system claim incoherent once security is removed from local jurisdiction?',
    'Legal structural analysis: Compare the scope of ''national security'' in the PRC''s interpretation (currently: subversion, secession, collusion, unlawful foreign coordination) versus the scope of matters Hong Kong retains autonomous authority over. If national security categories expand to cover political speech, civil-society coordination, and media criticism, Hong Kong''s autonomous system has no substantive legislative or judicial space. If national security is narrowly construed (espionage, military threat, sabotage of critical infrastructure), the two systems are structurally viable. Track how broadly the categories are applied in prosecution patterns.',
    'If security is broad enough to subsume political speech, the two-system claim is theater—there is one system (mainland authority) with a Hong Kong administrative apparatus. If security is narrow, the claim has substance. This frames whether the constraint is structurally TANGLED ROPE (both systems function but extraction occurs) or SNARE (the second system is facade).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_substantivity_question, conceptual, 'Whether the two-system structure can be coherent given mainland security override scope.').

omega_variable(
    treaty_interpretation_authority,
    'Who has the authority to interpret the One Country, Two Systems commitment: the PRC unilaterally, or joint signatories and international treaty bodies?',
    'International law precedent and treaty practice: Compare PRC''s unilateral reinterpretation of the Joint Declaration with other states'' treaty obligations. Hong Kong was handed over under an international agreement between the UK and PRC, witnessed by the UN, with signatory commitments to autonomy and civil liberties. If the PRC can unilaterally reinterpret those commitments, the treaty has no binding force outside PRC will. If treaty bodies or international law precedent constrain interpretation, the autonomy guarantees are enforceable through external pressure. Courts in third countries have begun accepting jurisdiction over Hong Kong cases under universal human-rights principles; this is a proxy test of whether the treaty''s autonomy commitments are enforceable.',
    'If the PRC''s unilateral interpretation is binding, the autonomy-primacy reading is structurally impossible—sovereignty-primacy is the only viable reading. If treaty bodies have co-authority, the autonomy-primacy reading remains live and this reading''s forecloses claim is false. This frames whether the contest is about reading a real kernel or about PRC assertion of absolute supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_interpretation_authority, conceptual, 'Whether treaty interpretation authority is unilateral PRC prerogative or shared with international signatories.').

omega_variable(
    extraction_vs_protection_claim,
    'Does the National Security Law''s expansion into speech and political coordination represent protection of national security, or protection of mainland regime''s political control?',
    'Comparative security analysis: Compare threat severity (actual plots, military infiltration, espionage) to prosecution volume and targets. Are prosecutions concentrated on genuine security threats, or are they concentrated on democratic opposition, media criticism, and civil-society coordination that poses no military threat? Examine confessions and trial evidence: what actual harms were prevented by each prosecution? If prosecution volume far exceeds threat severity, and targets are consistently opposition figures rather than security threats, the law is extractive (protecting regime control) not protective (defending security).',
    'If the National Security Law is deployed for regime-control extraction, the constraint is SNARE. If it is deployed proportionally to genuine threats, the constraint may be legitimate tangled rope (coordination + some asymmetry). This determines whether beneficiaries are genuinely coordinating or simply extracting under cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_protection_claim, empirical, 'Whether National Security Law enforcement is proportional to actual security threats or deployed to suppress political opposition.').

omega_variable(
    kernel_reading_contest_persistence,
    'Is the contest between these three readings unresolved, or has one reading been foreclosed by historical events?',
    'Observe whether each reading remains internally coherent and is still held by significant constituencies. Autonomy-primacy is still held by: Hong Kong civil society, international human-rights bodies, some Hong Kong judges and lawyers. Balanced-coexistence is still held by: some academics, some business leaders seeking compromise, some analysts. Sovereignty-primacy is held by: PRC central authority, mainland security apparatus, some Hong Kong-elite business figures. The contest is UNRESOLVED if all three remain live. It becomes FORECLOSED only if the PRC''s actual enforcement practices make one reading logically impossible (e.g., if autonomy were genuinely restored, autonomy-primacy would foreclose sovereignty-primacy). Current practice: the PRC enforces sovereignty-primacy through the National Security Law, but does not formally foreclose autonomy-primacy (it denies the contest exists, rather than engaging it). This is a sign that the kernel remains contested in the realm of legitimacy claims, even as one reading has enforcement dominance.',
    'If the contest is foreclosed, one reading becomes THE constraint and the others are historical artifacts. If unresolved, this constraint remains one option among three, disputed by different stakeholders. An unresolved kernel contest is Ω_C (conceptual indeterminacy), a sign that the constraint''s classification depends on which reading is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_persistence, conceptual, 'Whether the three readings coexist as live options or whether historical enforcement has foreclosed some.').

omega_variable(
    mainland_enforcement_scope_expansion,
    'Will mainland security authorities'' operational scope in Hong Kong continue to expand beyond the National Security Law''s formal scope?',
    'Track institutional creep: Monitor whether mainland law-enforcement agencies (Public Security Bureau, Ministry of State Security, armed services) expand their practical jurisdiction in Hong Kong beyond the Liaison Office''s formal authority. Early signals: secret detention facilities outside Hong Kong legal system, enforcement actions by mainland agents on Hong Kong citizens abroad, extension of mainland legal concepts (political crimes, ideological deviation) into Hong Kong prosecutions. If expansion continues, the two-system structure will erode functionally even if formal autonomy claims persist.',
    'If scope expansion continues, the theater_ratio will rise as Hong Kong autonomy becomes purely ceremonial. If expansion plateaus, Hong Kong retains some functional autonomy. This affects whether the constraint remains structurally tangled_rope (genuine two-system with extraction) or devolves into snare (one-system theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainland_enforcement_scope_expansion, empirical, 'Whether mainland enforcement scope will exceed statutory authority through institutional creep.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 1997, 0.08).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(one__tr_t2009, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2009, 0.12).
narrative_ontology:measurement(one__tr_t2015, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(one__tr_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(one__be_t2009, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement(one__be_t2015, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(one__be_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2025, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 1997, 0.42).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2003, 0.45).
narrative_ontology:measurement(one__su_t2009, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2009, 0.48).
narrative_ontology:measurement(one__su_t2015, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(one__su_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.18).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society_identity_lock__mainland_integration_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judicial_independence__security_exception_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_electoral_system__patriotism_gate_reading).

% DUAL FORMULATION NOTE:
% The one_country_two_systems_framework kernel has three structural readings, each generating a distinct constraint with its own ε and stakeholder structure. (1) This constraint—sovereignty_primacy_reading—models the binding interpretation: PRC central authority has unilateral power to define and override autonomy. (2) autonomy_primacy_reading (separate file) models Hong Kong autonomy as treaty-protected and substantive, with international enforcement. (3) balanced_coexistence_reading (separate file) models the system as requiring continuous political negotiation without legal supremacy of either level. These are not the same constraint viewed from different angles; they are different constraints instantiated from the same formal kernel. The ε values differ substantially: sovereignty-primacy is highly extractive (0.81); autonomy-primacy is low-extraction coordination (expected ~0.25-0.35); balanced-coexistence is moderate-extraction with genuine coordination (expected ~0.45-0.55). The measurement series document the historical trajectory toward sovereignty-primacy enforcement; the sibling readings document counterfactual trajectories if other legal interpretations had prevailed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, powerful, 0.45).
constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
