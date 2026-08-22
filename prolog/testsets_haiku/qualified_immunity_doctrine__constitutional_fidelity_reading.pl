% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   Qualified immunity is a judicial doctrine created in 1982 (Harlow v.
 *   Fitzgerald) that shields law enforcement officers from damages liability
 *   when they violate constitutional rights, provided the violated right was
 *   not 'clearly established' in settled law at the time of the violation.
 *   Under this reading—the constitutional fidelity reading—the doctrine is
 *   illegitimate because it lacks authorization in constitutional text,
 *   statutory law, or prior doctrine. The judiciary fabricated it whole to
 *   expand institutional power (gatekeeping remedies) and to shield law
 *   enforcement from accountability. The doctrine operates as pure
 *   extraction: victims of constitutional violations are denied judicial
 *   remedy; officers are insulated from the liability rules that existed for
 *   two centuries before 1982; and the judiciary retains exclusive authority
 *   to define what rights are 'clearly established' (a moving target that
 *   systematically forecloses liability). This reading asserts the entire
 *   doctrine is unlawful regardless of policy consequences—a legitimacy
 *   claim, not a utilitarian one.
 *
 * KEY AGENTS:
 *   - Federal judiciary: Institutional agenda-setter; creates and maintains the doctrine; expands its own power by gatekeeping remedies
 *   - Law enforcement officers: Primary beneficiaries; gain near-absolute impunity for constitutional violations
 *   - Constitutional plaintiffs & victims of unlawful force: Structural targets; denied remedy by motion practice; trapped exit
 *   - Congress: Excluded; has no legislative role in a common-law doctrine
 *   - State attorneys general: Dual-positioned; benefit institutionally (avoid liability), pay through indemnification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.71).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '791a3d59-c0cb-4bb8-a966-0b632ff2f129').
narrative_ontology:cs_kernel_codification('791a3d59-c0cb-4bb8-a966-0b632ff2f129', fixed_text).
narrative_ontology:cs_authority_grounding('791a3d59-c0cb-4bb8-a966-0b632ff2f129', extraction).
narrative_ontology:cs_interpretation_layer_present('791a3d59-c0cb-4bb8-a966-0b632ff2f129').
narrative_ontology:cs_reading_relation('791a3d59-c0cb-4bb8-a966-0b632ff2f129', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('791a3d59-c0cb-4bb8-a966-0b632ff2f129', qualified_immunity_doctrine__accountability_void_reading, influences).
narrative_ontology:cs_axiom('791a3d59-c0cb-4bb8-a966-0b632ff2f129', foundational, judicial_authority_foreclosed_by_section_1983).
narrative_ontology:cs_axiom_status(judicial_authority_foreclosed_by_section_1983, holdable).
narrative_ontology:cs_axiom_grounding('791a3d59-c0cb-4bb8-a966-0b632ff2f129', judicial_authority_foreclosed_by_section_1983, empirically_contingent).
narrative_ontology:cs_axiom('791a3d59-c0cb-4bb8-a966-0b632ff2f129', foundational, remedy_inseparable_from_constitutional_right).
narrative_ontology:cs_axiom_status(remedy_inseparable_from_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('791a3d59-c0cb-4bb8-a966-0b632ff2f129', remedy_inseparable_from_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('791a3d59-c0cb-4bb8-a966-0b632ff2f129', constitutional_text_section_1983_statutory_right).
narrative_ontology:cs_drift_state('791a3d59-c0cb-4bb8-a966-0b632ff2f129', contemporary_post_harlow_institutional_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('791a3d59-c0cb-4bb8-a966-0b632ff2f129', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_unlawful_force).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, state_attorney_general).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, state_attorney_general).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Creates, maintains, and interprets qualified immunity doctrine through case law. Holds exclusive authority to adjudicate whether officers had 'clearly established' constitutional rights against which to judge conduct. Expands its own institutional power by denying plaintiffs a venue for accountability while retaining the appearance of constitutional adjudication. Acts as gatekeeper of remedies without statutory authorization.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Gain near-absolute insulation from personal liability for constitutional violations under color of law. Can suppress speech, conduct unlawful searches, apply excessive force, and deprive liberty without judicial scrutiny so long as the violated right was not 'clearly established' at the time. Exit available through voluntary compliance with constitutional norms, but the doctrine removes the enforcement mechanism that would compel it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary,
    organized, biographical, mobile, national).

% Seek judicial vindication of constitutional rights violated by officers, but face an insurmountable pleading burden: must show not only that a constitutional right exists but that it was 'clearly established' in settled law at the moment of the violation. The doctrine's retroactive application—requiring pre-existing precedent before liability attaches—means novel violations categorically escape remedy. Must navigate federal court without resources; most cannot afford counsel to survive motion to dismiss.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Suffer direct constitutional harms—unlawful arrest, excessive force, wrongful death—and are categorically denied remedy through the courts. The doctrine operates to exclude their injuries from the judicial record entirely: cases are dismissed before evidence is heard. Their only recourse is legislative action (outside the courts) or settlement (which the doctrine discourages by removing incentive to settle); both are structural alternatives to the court system, not functional remedies within it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_unlawful_force, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_unlawful_force, excluded).

% Indemnifies officer defendants at state cost (most states), shifting the cost of constitutional violations from individuals to the public fisc. This makes the state a structural beneficiary (avoids individual liability rules that would discipline hiring/retention) while also a payer (bears the indemnification cost). The constraint allows the state to avoid institutional accountability for hiring and retaining violative officers.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, state_attorney_general, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, state_attorney_general, payer).

% Seek to litigate constitutional harms on behalf of victims and to establish the precedent needed to overcome the 'clearly established' hurdle. Excluded by motion practice that resolves cases before discovery; can influence outcomes only through legislative advocacy or congressional testimony, not through the judicial process the doctrine controls.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Has never authorized qualified immunity by statute; the doctrine is entirely judicially created. Excluded from the primary legal framework because it resides in common law (case-constructed, not legislated). Retains theoretical power to abolish the doctrine through statute, but the judiciary's gatekeeping authority over constitutional meaning prevents legislative action from fully displacing the doctrine without constitutional amendment or sustained legislative override.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, analytical, national).

% The Fourth Amendment (unlawful search/seizure), Fifth Amendment (due process), Fourteenth Amendment (equal protection, due process) all establish rights to remedies for violations. The doctrine operates to sever the right from the remedy, a structural inversion the text does not authorize. Included as a non-agent entity (a vindicated proposition) for completeness.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The doctrine does not solve a coordination problem. Law enforcement predates qualified immunity by two centuries; officers operated under liability rules before 1982 without systemic collapse. The stated coordination rationale—that liability exposure would paralyze policing—is post-hoc justification, not a founding function.
% TRANSFER_FUNCTION: Transfers accountability from individual officers and hiring institutions to no one. Constitutional violations become unremedied private injuries. The doctrine redistributes power from plaintiffs to judiciary (institutional authority expansion) and from victims to officers (impunity), but does not transfer resources or obligations—it removes both.
% ABSENT_VOICES: Constitutional plaintiffs and victims of unlawful force are structurally excluded: they are dismissed before being heard. Civil rights organizations are excluded from the primary judicial process and forced to the legislative sphere. Congress is excluded from the framework entirely—the doctrine resides in common law created by courts, not in statute. A competing voice—originalist scholars and judges who argue the doctrine lacks constitutional warrant—is present in dissent but excluded from controlling doctrine.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished, officers would return to existing common-law and statutory liability frameworks that governed before 1982. Constitutional remedies would attach to constitutional violations. Police departments would adjust hiring, training, and oversight to comply with constitutional norms or face institutional liability. The entire accountability ecology would reorganize around actual constitutional constraints rather than judicial fiat exemption.
% FOUNDING_PROBLEM: In Harlow v. Fitzgerald (1982), the Supreme Court created qualified immunity by stating (without constitutional or statutory warrant) that policy required shielding officers from damages liability to enable 'vigorous' law enforcement. No constitutional text, no statute, and no prior doctrine authorized the holding. The court invented the doctrine wholesale to solve a problem (fear of liability) that it identified, not one that existed in statute or constitutional text.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists (Thomas, Gorsuch) have stated the founding problem was misidentified—no constitutional warrant exists for immunity and the doctrine was never authorized. Historians and constitutional scholars outside the judiciary (Schwartz, Baude, Adler, Calabresi) attest the doctrine lacks historical or textual foundation. Two centuries of pre-1982 law enforcement operated under liability rules without the systemic collapse Harlow predicted. The problem Harlow cited is not corroborated outside the judiciary's own institutional interest in expansion.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under this reading, extractiveness is measured against the standard that constitutional violations are entitled to remedies (the constitutional promise itself, not a policy outcome). The doctrine extracts by severing right from remedy. Suppression is high (0.71) because enforcement depends on motion practice that dismisses cases before plaintiffs are heard—they never reach a jury or live court. Theater is substantial (0.58) because the doctrine performs constitutional reasoning (citing the Fourth Amendment, etc.) while operating to deny the very remedy the Constitution promises. Accessibility collapse is moderate (0.67) because victims theoretically retain legislative recourse, but practically the judiciary's gatekeeping power over constitutional meaning prevents legislative remedies from operating. Resistance is high (0.73) because victims' organizations, scholars, and now sitting justices (Thomas, Gorsuch) actively challenge the doctrine's legitimacy. The measurement series show extraction rising over 44 years as courts systematically narrowed 'clearly established' doctrine to foreclose more cases—the doctrine's entrenchment increased its extractiveness. Theater ratio rose as the judiciary invested more in justifying the doctrine rhetorically as enforcement becomes harder to defend.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and officers experience this doctrine as legitimate protection; the reading rejects that entire frame as illegitimate fabrication. Victims and plaintiffs experience it as categorical denial of remedy—a structural absence of justice, not a procedural hurdle. This is not a disagreement about metrics; it is a disagreement about the fundamental legitimacy of the framework. The engine will compute the judiciary's seat as beneficiary (d near 0.0) and the victim's seat as target (d near 1.0), but the deeper disagreement is whether the framework itself (the doctrine) is a legitimate constraint at all. Under this reading, both seats are denied a legitimate legal framework—the doctrine is ultra vires (beyond judicial authority), so neither seat operates within legitimate constitutional bounds. This is distinct from other readings where the doctrine is contested on policy grounds (whether it achieves its stated aims) or extraction grounds (whether the cost is justified by coordination benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary derives d ≈ 0.0 to 0.15 (full beneficiary): it sets the doctrine, interprets it, controls the 'clearly established' gate, and expands institutional authority. Officers derive d ≈ 0.10 to 0.25 (net beneficiary): they gain impunity but have no formal role in doctrine creation and could theoretically oppose it (though the doctrine benefits them). Constitutional plaintiffs derive d ≈ 0.90 to 1.0 (full target): they bear the extraction (denial of remedy) and have no exit. Victims of unlawful force derive d ≈ 1.0 (full target): they are directly targeted and trapped. Congress derives d ≈ 0.5 to 0.65 (asymmetric): excluded from the primary framework, unable to legislatively displace the doctrine without constitutional amendment or sustained override, but not directly extracted from. State attorneys general derive d ≈ 0.35 to 0.50 (beneficiary with cost): they avoid individual liability but pay through indemnification—benefits concentrated (institutional impunity) and costs diffuse (public fisc).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear that liability exposure would paralyze policing) is dead on this reading. Two centuries of pre-1982 law enforcement operated under liability rules without systemic collapse. The doctrine persists not because the problem it was invented to solve remains live, but because it benefits the judiciary (power expansion) and officers (impunity). The doctrine's persistence despite the founding problem's death is the signature of mandatrophy—institutional inertia and institutional interest sustaining a rule whose original justification has evaporated. Under the constitutional fidelity reading, mandatrophy is the entire story: a fabricated doctrine sustains itself through institutional interest, not through any live function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_legitimacy_vs_policy_assessment,
    'Is the operative disagreement about qualified immunity a disagreement about judicial authority (did the courts have power to create it?), or about policy outcomes (does the doctrine achieve desirable law enforcement)?',
    'Originalist and textualist jurisprudence: if no constitutional text or prior doctrine authorizes the doctrine, then the disagreement is about authority, not outcomes. Legislative history of Section 1983 (which authorized suits against officers) would show whether Congress intended qualified immunity as an exception.',
    'If the disagreement is about authority, then the constitutional fidelity reading is distinct from the protective_scaffold reading (which accepts judicial authority but contests policy outcomes). If outcomes-driven, the readings collapse into a dispute about whether protection is worth the cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framework_legitimacy_vs_policy_assessment, conceptual, 'Whether qualified immunity is contested on grounds of judicial authority or policy efficiency.').

omega_variable(
    clearly_established_doctrine_gate_function,
    'Does the ''clearly established law'' requirement operate as a genuine protection for reasonable officer conduct, or as a systematically manipulated gate that forecloses liability ex-post?',
    'Empirical analysis: compare district courts'' rate of dismissal on clearly-established grounds before and after specific appellate rulings narrowing the doctrine. If the gate tightens systematically with each appellate decision, the requirement functions as a mechanism to foreclose liability rather than to protect reasonable conduct.',
    'If the gate is systematically manipulated, suppression_requirement measurement would be vindicated as the operative enforcement mechanism (courts dismiss cases before hearing evidence). If the gate is a legitimate reasonable-conduct standard, suppression measurement would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_doctrine_gate_function, empirical, 'Whether the clearly established requirement protects reasonable conduct or systematically forecloses liability.').

omega_variable(
    victim_suppression_internalized_or_structural,
    'Is suppression of victims'' claims structural (courts dismiss via motion practice) or internalized (victims believe they have no remedy and do not file suit)?',
    'Survey of constitutional plaintiffs and victims'' organizations about why suits are not filed; analysis of case filing trends in pre-1982 and post-1982 eras; state jurisdictions that abolished qualified immunity to observe if filing and settlement patterns change.',
    'If suppression is primarily structural (motion dismissals), then it is a feature of the doctrine itself and persists if the doctrine persists. If internalized (victims do not sue), then the doctrine''s suppressive power exceeds the motion-dismissal rate—victims carry the suppression with them even if legal barriers fell.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_suppression_internalized_or_structural, empirical, 'Whether victims'' suppression is structural (legal barriers) or internalized (belief in lack of remedy).').

omega_variable(
    judicial_authority_to_recognize_immunities,
    'Did the courts have common-law authority to recognize qualified immunity as a blanket exception to constitutional liability, or did Section 1983 (enacted 1871) foreclose that authority by creating a statutory right of action?',
    'Originalist and textualist interpretation: Section 1983 explicitly authorized suits against officers acting under color of law. If the statute intended to create a right of action, then courts could not later create a blanket exception to that right without amending the statute. Historical analysis of common-law immunity doctrine (which existed for executive officials in narrow contexts) would show whether a blanket immunity for law enforcement was ever recognized.',
    'If courts lacked authority, the doctrine is void on first principles and neither seat operates within legitimate constitutional bounds. If courts had authority, the reading collapses into an accuracy dispute about whether they exercised it correctly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_to_recognize_immunities, conceptual, 'Whether Section 1983 foreclosed judicial power to create blanket immunities.').

omega_variable(
    kernel_contest_between_readings,
    'Can the protective_scaffold_reading and the constitutional_fidelity_reading coexist within a single legal framework, or does one reading foreclose the other?',
    'Logical analysis: if the constitutional fidelity reading is correct (courts lacked authority), then the protective_scaffold reading is built on an illegitimate foundation and cannot coexist within constitutional law. If the constitutional fidelity reading is wrong (courts did have authority), then both readings can coexist as empirical disputes about whether the doctrine works.',
    'If they foreclose, this constraint and the protective_scaffold constraint are mutually exclusive instantiations of the same kernel. If they coexist, they are live alternatives in public discourse. The reading_relations field in cs_structure declares the structural answer (see below).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_between_readings, conceptual, 'Logical relationship between constitutional_fidelity_reading and protective_scaffold_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(qual_tr_t1990, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(qual_tr_t2000, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(qual_tr_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(qual_tr_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2020, 0.54).
narrative_ontology:measurement(qual_tr_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement(qual_be_t1990, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(qual_be_t2000, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(qual_be_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(qual_be_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement(qual_be_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2026, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(qual_su_t1990, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(qual_su_t2000, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(qual_su_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(qual_su_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(qual_su_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.0).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_civil_rights_cause_of_action).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_expansion_harlow_carver_heien_chain).

% DUAL FORMULATION NOTE:
% The qualified_immunity_doctrine kernel generates three distinct constraint stories, one per reading. This story (constitutional_fidelity_reading) asserts the doctrine is illegitimate due to judicial ultra vires creation. The protective_scaffold_reading argues the doctrine is legitimate policy even if contested. The accountability_void_reading accepts the doctrine's existence but measures its extractive function. All three share the same referent (the doctrine's operation) but diverge on legitimacy frame. They are linked via network.affects_constraints to enable cross-reading comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
