% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Laws as Higher-Order Framework: Judicial Supremacy Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_supremacy_reading of the
 *   basic_law_interpretive_boundary kernel. Under this reading, the Basic
 *   Laws (enacted 1992 onward) constitute a higher-order constitutional
 *   framework that the Supreme Court is duty-bound to interpret and enforce.
 *   Judicial invalidation of Knesset legislation that contradicts Basic Laws
 *   is binding on the legislature. The Court becomes the active
 *   constraint-enforcer; Knesset majorities become constrained targets;
 *   rights-claimants gain a litigation veto. The claimed type is
 *   tangled_rope: genuine coordination (rights protection, constitutional
 *   stability) coexists with asymmetric extraction (legislative majorities
 *   blocked, security establishment constrained). The other two readings of
 *   this kernel — parliamentary_sovereignty_reading and
 *   balanced_contestation_reading — are separate constraints with their own ε
 *   values and structural profiles, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda_setter (institutional/analytical) — interprets and enforces Basic Laws, nullifies legislation
 *   - knesset_majority_coalitions: payer (powerful/constrained) — legislation subject to judicial nullification, limited exit via override
 *   - rights_claimants: beneficiary (powerless/identity_locked) — gain litigation veto over rights-violating laws, no alternative forum
 *   - government_ministries: payer (organized/constrained) — policy implementation constrained by judicial review
 *   - security_establishment: payer (institutional/constrained) — security measures subject to proportionality review
 *   - civil_society_organizations: beneficiary (organized/mobile) — use litigation to enforce rights, some exit via international fora
 *   - legal_academy: observer (analytical/analytical) — analyzes and legitimates/delegitimates the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.45).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Laws as Higher-Order Framework: Judicial Supremacy Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '33201a65-63bd-47d0-98ab-f3e2451f3395').
narrative_ontology:cs_kernel_codification('33201a65-63bd-47d0-98ab-f3e2451f3395', formalized).
narrative_ontology:cs_authority_grounding('33201a65-63bd-47d0-98ab-f3e2451f3395', lineage).
narrative_ontology:cs_interpretation_layer_present('33201a65-63bd-47d0-98ab-f3e2451f3395').
narrative_ontology:cs_reading_relation('33201a65-63bd-47d0-98ab-f3e2451f3395', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('33201a65-63bd-47d0-98ab-f3e2451f3395', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('33201a65-63bd-47d0-98ab-f3e2451f3395', foundational, basic_laws_entrenched_superstatutes).
narrative_ontology:cs_axiom_status(basic_laws_entrenched_superstatutes, holdable).
narrative_ontology:cs_axiom_grounding('33201a65-63bd-47d0-98ab-f3e2451f3395', basic_laws_entrenched_superstatutes, conventional).
narrative_ontology:cs_axiom('33201a65-63bd-47d0-98ab-f3e2451f3395', foundational, court_final_interpreter_of_constitutional_meaning).
narrative_ontology:cs_axiom_status(court_final_interpreter_of_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('33201a65-63bd-47d0-98ab-f3e2451f3395', court_final_interpreter_of_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('33201a65-63bd-47d0-98ab-f3e2451f3395', secondary, proportionality_as_universal_rights_test).
narrative_ontology:cs_axiom_status(proportionality_as_universal_rights_test, holdable).
narrative_ontology:cs_axiom_grounding('33201a65-63bd-47d0-98ab-f3e2451f3395', proportionality_as_universal_rights_test, instrumental).
narrative_ontology:cs_reference_frame('33201a65-63bd-47d0-98ab-f3e2451f3395', mizrahi_bank_constitutional_revolution).
narrative_ontology:cs_drift_state('33201a65-63bd-47d0-98ab-f3e2451f3395', contemporary_judicial_reform_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('33201a65-63bd-47d0-98ab-f3e2451f3395', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, civil_society_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, government_ministries).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, security_establishment).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, judicial_review_as_constitutional_guardian).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_laws_entrenched_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Basic Laws through judicial review; nullify Knesset legislation that contradicts Basic Laws; develop proportionality and reasonableness doctrines. Their authority derives from the 1995 Mizrahi Bank decision establishing constitutional supremacy. They face no domestic exit — their role is constitutionally entrenched — but participate in international judicial networks.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, generational, analytical, national).

% Enact legislation by majority vote; legislation subject to judicial nullification if inconsistent with Basic Laws. Override requires either Basic Law amendment (61 MKs) or proposed override clause (61 MKs, limited duration). Political cost of override is high — charges of constitutional vandalism, international criticism. Exit from the constraint means abandoning legislative agenda or accepting judicial veto.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalitions, payer,
    powerful, biographical, constrained, national).

% Individuals and groups whose rights under Basic Laws (human dignity, freedom of occupation, equality) are threatened by legislation or state action. Access the Court via petitions; the Court's veto is their primary protection. Exit is identity_locked: their conception of themselves as rights-bearers is constituted through the Court's jurisdiction — leaving the system means abandoning the rights-claiming identity. No alternative domestic forum exists.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, biographical, identity_locked, national).

% Implement policy through regulations and administrative decisions subject to judicial review on proportionality, reasonableness, and Basic Law compliance. Policy space narrowed by Court's expanding doctrine. Exit means policy paralysis or legislative override (politically costly). Some ministries (Justice) have dual role as Court's institutional partner.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, government_ministries, payer,
    organized, biographical, constrained, national).

% Military, intelligence, and police operations subject to judicial review on proportionality and human dignity grounds. Court reviews targeted killings, detention, surveillance, movement restrictions. Security establishment argues Court lacks expertise; Court asserts constitutional supervision. Exit constrained: operational necessity vs. legal compliance; political cover for non-compliance is risky.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, security_establishment, payer,
    institutional, biographical, constrained, national).

% NGOs, advocacy groups, legal clinics that litigate to enforce Basic Law rights. Fund litigation, submit amicus briefs, mobilize public opinion. Benefit from Court's expansive standing and remedial powers. Exit is mobile: can shift advocacy to international human rights bodies (UN treaty bodies, ICC), but domestic Court remains primary forum for immediate relief.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, national).

% Scholars who analyze, critique, and legitimize/delegitimize the judicial supremacy reading. Produce the doctrinal vocabulary (constitutional revolution, dialogue theory, constitutional complaint). Divided between defenders (constitutionalism requires supremacy) and critics (democratic legitimacy requires legislative finality). Their exit is analytical — they observe from outside the constraint's operative force.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable higher-order legal framework protecting fundamental rights against majoritarian legislation, providing a single authoritative interpreter (the Court) to prevent legislative chaos and rights erosion.
% TRANSFER_FUNCTION: Moves legislative decision-making authority from the Knesset (elected majority) to the Supreme Court (unelected judges) on any matter touching Basic Law rights — effectively a veto transfer on rights-impacting legislation.
% ABSENT_VOICES: Palestinian citizens of Israel (systematically underrepresented in Knesset, Court appointments, and civil society funding) and Jewish settlers in West Bank (whose property/land claims are often overridden by Court on proportionality grounds) — both would object to the current arrangement but occupy excluded seats with different grievances. Ultra-Orthodox parties (whose draft exemption laws are repeatedly struck down) are partially represented in Knesset but excluded from Court's interpretive community.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the Knesset would regain final interpretive authority over Basic Laws. Rights-claimants would lose their litigation veto. The security establishment would gain operational autonomy. The constitutional order would shift from judicial supremacy to parliamentary sovereignty — a fundamental rearrangement of Israeli constitutional politics.
% FOUNDING_PROBLEM: Post-1992: Israel lacked a formal constitution with entrenched rights. The Basic Laws were enacted as ordinary statutes (simple majority, no entrenchment). The founding problem was how to give these laws constitutional force without a constituent assembly or formal constitutional enactment process.
% FOUNDING_PROBLEM_CORROBORATION: The Court (Barak, Mizrahi Bank decision) attests the founding problem is live — constitutional supremacy requires active judicial enforcement. Knesset majorities (2023 judicial reform coalition) and legal critics (Rubinstein, Shaked) attest the founding problem was solved by enactment itself — Basic Laws are constitutional by virtue of Knesset's constituent authority, not judicial elevation. International scholars (Hirschl, Ginsburg) corroborate the 'constitutional revolution' was judicial, not legislative.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the Court's power to nullify legislation — a substantial transfer of decision-making authority from elected legislature to unelected court. The trajectory shows extraction rising from 1992 (Basic Law enactment, pre-revolution) through the 1995 Mizrahi Bank constitutional revolution, peaking around 2000-2006 (height of judicial activism), a slight dip during the 2006-2015 period (more deferential Court), then rising again to 2023 (judicial reform crisis). Theater ratio rises gradually (0.10→0.28) as performative 'dialogue' rhetoric increases while substantive constraint hardens. Suppression requirement (0.20→0.45) tracks the Court's growing reliance on active enforcement: proportionality doctrine, reasonableness standard, and the override clause debate. Accessibility collapse (0.55) is moderate — alternatives exist (political override, constitutional amendment) but are politically costly. Resistance (0.78) is high — sustained political, academic, and public contestation over the Court's authority.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat (agenda_setter, institutional, analytical exit), the constraint is genuine coordination: protecting constitutional order from majoritarian excess. From Knesset majority coalitions (payer, powerful, constrained exit), it is extraction: their legislative agenda is vetoed by an unelected body. From rights_claimants (beneficiary, powerless, identity_locked), it is their only effective protection — exit is impossible because their rights-claiming identity is fused with the Court's jurisdiction. From security_establishment (payer, institutional, constrained), it is operational constraint: security decisions subject to judicial second-guessing. The engine computes these seat-level types from the structural data; the claimed tangled_rope reflects the aggregate asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: rights_claimants (identity_locked — their rights-claiming self-concept is constituted through Court access), supreme_court_justices (institutional power, arbitrage-grade exit via international judicial networks), civil_society_organizations (organized, mobile — can shift to international fora). Victims declared: knesset_majority_coalitions (powerful but constrained — override requires supermajority or political capital), government_ministries (organized, constrained — policy space narrowed), security_establishment (institutional, constrained — operational autonomy reduced). The directionality derivation assigns low d to beneficiaries (subsidy), high d to victims (extraction amplified). Override not needed — structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1992: establishing constitutional supremacy in a system without a formal constitution) remains contested — not dead. The Court's mandate has expanded from rights-protection to policy-supervision (reasonableness doctrine, proportionality in security affairs). This expansion is contested as mandatrophy: the original coordination function (rights guardrails) has accumulated extractive layers (policy veto). The reading's axiomatic claim (constitutional_supremacy_doctrine) remains holdable but faces active repudiation_pressure. The mandatrophy is unresolved — the constraint persists in expanded form without renewed democratic authorization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the judicial supremacy reading a distinct constraint from the parliamentary sovereignty and balanced contestation readings, or a different measurement of the same constraint?',
    'Apply the ε-invariance test: if changing the observable (which institution has final interpretive authority) changes ε, they are different constraints. This reading instantiates a specific ε (0.62) for the standing arrangement where Court nullification binds Knesset — the other readings instantiate different ε values for different standing arrangements.',
    'Confirms this is one reading of kernel ''basic_law_interpretive_boundary'', not a parameterization of a single constraint. Links to sibling constraints via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to this reading as a distinct ε-invariant constraint in the kernel family').

omega_variable(
    basic_laws_naturalness,
    'Are the Basic Laws a genuine higher-order constitutional framework (mountain-like natural law) or a constructed political arrangement that benefits identifiable agents (tangled_rope)?',
    'Historical analysis of Basic Law enactment: were they enacted as constitutional super-statutes with entrenchment, or as ordinary legislation later elevated by judicial interpretation? The 1992 Basic Laws (Human Dignity, Freedom of Occupation) were enacted by simple majority without formal entrenchment clauses — the ''constitutional revolution'' was judicial.',
    'If constructed, the judicial supremacy reading is a false summit candidate (Mountain claim with beneficiaries) — FSM would trigger. If genuine constitutional framework, the high extractiveness is coordination cost, not rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_laws_naturalness, empirical, 'Whether the higher-order status of Basic Laws is intrinsic or judicially constructed').

omega_variable(
    extraction_distribution,
    'Does the extraction (ε=0.62) fall primarily on legislation threatening court-protected liberties, or does it extend to routine governance?',
    'Case-level analysis of judicial invalidation frequency and subject matter: proportion of struck-down laws involving civil liberties vs. economic regulation, security policy, religious affairs.',
    'If extraction concentrates on liberty-threatening legislation, the coordination function (rights protection) is genuine and extraction is targeted. If extraction is broad, the constraint is closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_distribution, empirical, 'Sectoral distribution of judicial nullification''s extractive burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blib_jsr_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(blib_jsr_tr_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(blib_jsr_tr_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(blib_jsr_tr_t2006, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(blib_jsr_tr_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(blib_jsr_tr_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(blib_jsr_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(blib_jsr_be_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(blib_jsr_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(blib_jsr_be_t2006, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2006, 0.52).
narrative_ontology:measurement(blib_jsr_be_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(blib_jsr_be_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2023, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(blib_jsr_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(blib_jsr_su_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(blib_jsr_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(blib_jsr_su_t2006, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2006, 0.42).
narrative_ontology:measurement(blib_jsr_su_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(blib_jsr_su_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2023, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, israeli_judicial_reform_crisis).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_human_dignity_interpretation).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_freedom_of_occupation_interpretation).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'Basic Laws interpretive boundary' kernel. The judicial_supremacy_reading (this story) has ε=0.62, claimed_type=tangled_rope. The parliamentary_sovereignty_reading would have lower ε (Knesset unconstrained) but high suppression for rights-claimants. The balanced_contestation_reading would have intermediate ε with complex seat divergence. All three linked via affects_constraints. The ε-invariance principle requires separate stories because changing the observable (which institution has final say) changes ε fundamentally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
