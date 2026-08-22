% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: Hong Kong National Security Law as Jurisdictional Transplantation Vehicle
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This story reads the National Security Law as a vehicle for transplanting
 *   mainland civil-law administrative and interpretive doctrine into Hong
 *   Kong's common law jurisdiction under the formal cover of a single
 *   security statute. The reading focuses specifically on institutional
 *   architecture — designated judges, NPCSC interpretive supremacy, the
 *   Office for Safeguarding National Security's parallel jurisdiction,
 *   restricted foreign counsel — rather than on the criminalization of
 *   dissent per se (that is the democratic_enclosure_reading, a separate
 *   constraint) or on the sovereignty-restoration justification for the law's
 *   existence (the sovereignty_restoration_reading, also separate). The
 *   distinguishing structural claim here is jurisdictional: common law
 *   procedural and interpretive norms are being displaced by mainland
 *   legal-administrative logic through a mechanism that preserves the outward
 *   form of HK courts while altering who effectively controls outcomes in a
 *   defined case category.
 *
 * KEY AGENTS:
 *   - mainland_security_apparatus: institutional beneficiary — gains standing enforcement jurisdiction inside a formerly separate common law system
 *   - central_government_liaison_office: institutional beneficiary/agenda_setter — administers judge designation and case-track determination
 *   - hk_judiciary: institutional payer — operates a bifurcated system that erodes the independence and precedent-based reasoning that constituted its professional identity
 *   - hk_legal_profession: organized payer — bears procedural and reputational costs of practicing under transplanted doctrine
 *   - npc_standing_committee: institutional agenda_setter — holds unappealable interpretive supremacy, the clearest single mechanism of doctrinal transplantation
 *   - international_legal_observers: analytical observer — tracks the structural transplantation independent of either benefiting or paying party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.71).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "Hong Kong National Security Law as Jurisdictional Transplantation Vehicle").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '23abbef9-ea24-45cf-aa0e-c20bfc3591d9').
narrative_ontology:cs_kernel_codification('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', formalized).
narrative_ontology:cs_authority_grounding('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', extraction).
narrative_ontology:cs_interpretation_layer_present('23abbef9-ea24-45cf-aa0e-c20bfc3591d9').
narrative_ontology:cs_reading_relation('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', foundational, common_law_interpretive_autonomy_is_constitutive_of_one_country_two_systems).
narrative_ontology:cs_axiom_status(common_law_interpretive_autonomy_is_constitutive_of_one_country_two_systems, holdable).
narrative_ontology:cs_axiom_grounding('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', common_law_interpretive_autonomy_is_constitutive_of_one_country_two_systems, conventional).
narrative_ontology:cs_axiom('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', foundational, final_interpretive_authority_must_rest_with_the_adjudicating_jurisdictions_own_courts).
narrative_ontology:cs_axiom_status(final_interpretive_authority_must_rest_with_the_adjudicating_jurisdictions_own_courts, holdable).
narrative_ontology:cs_axiom_grounding('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', final_interpretive_authority_must_rest_with_the_adjudicating_jurisdictions_own_courts, conventional).
narrative_ontology:cs_reference_frame('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', common_law_judicial_autonomy_under_basic_law).
narrative_ontology:cs_drift_state('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', post_2020_nsl_enactment, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('23abbef9-ea24-45cf-aa0e-c20bfc3591d9', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, central_government_liaison_office).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, designated_judges_panel_administrators).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, common_law_litigants).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, foreign_qualified_barristers).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, one_country_two_systems_formal_continuity).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, national_security_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains a standing office in Hong Kong (the Office for Safeguarding National Security) with jurisdiction to try designated cases under mainland procedure, transfer cases to mainland courts, and operate outside HK judicial oversight in defined circumstances. Receives a parallel enforcement channel it did not previously have inside the common law jurisdiction.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter).

% Administers the certification process by which the Chief Executive designates judges eligible to hear national security cases, effectively filtering the judicial pool by a criterion (perceived reliability on security matters) foreign to common law judicial appointment norms. Shapes case allocation without appearing formally as a court.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, central_government_liaison_office, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, central_government_liaison_office, agenda_setter).

% Common law judges who built careers on precedent-based reasoning and judicial independence from the executive now operate a two-track system: ordinary common law jurisdiction for most matters, and a parallel security jurisdiction with executive-influenced judge designation, no jury for certain trials, and provisions allowing mainland courts to take precedence on interpretation. Cannot decline designation without professional consequence, and cannot appeal or contest an NSL interpretation issued by the NPC Standing Committee, which binds HK courts absolutely.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, payer,
    institutional, biographical, constrained, national).

% Barristers and solicitors who practiced under adversarial common law procedure now face bail provisions reversed from the ordinary presumption, restrictions on choice of foreign counsel in security cases, and professional risk in mounting vigorous defenses that could themselves be characterized as endangering security. Some have left the jurisdiction; those remaining self-censor case selection and argument strategy.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession, payer,
    organized, biographical, constrained, national).

% Defendants and civil parties whose cases may be reclassified into the security track lose access to jury trial, face different bail presumptions, and cannot predict in advance which procedural regime will govern their matter. Have no meaningful exit — they cannot choose their forum or contest designation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, common_law_litigants, payer,
    powerless, immediate, trapped, regional).

% Overseas-qualified counsel, historically permitted ad hoc admission to argue complex HK cases, are now barred from national security matters absent special certification tightly controlled by the Chief Executive. They can practice elsewhere but are foreclosed from precisely the highest-stakes cases where common law expertise would matter most, removing an external check on procedural drift.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, foreign_qualified_barristers, excluded,
    moderate, biographical, mobile, global).

% Holds final, unappealable interpretive authority over the NSL text itself. Any HK court ruling that conflicts with an NPCSC interpretation is superseded automatically. This is a mainland civil-law-tradition legislative interpretation power grafted onto what HK litigants and judges experience as a common law constitutional order — a transplant of doctrine, not merely of one statute.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, npc_standing_committee, agenda_setter,
    institutional, civilizational, analytical, national).

% Bar associations, UN human rights bodies, and comparative law scholars track case outcomes, judge designations, and the frequency of NPCSC interpretive interventions to assess whether HK's common law character is being structurally hollowed from within its formal shell.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating conduct the central government characterizes as threatening state security within a jurisdiction whose existing common law procedures (jury trial, open bail presumption, unrestrained defense counsel selection) were not designed with mainland security doctrine in mind — solving a genuine coordination problem for the central authority: how to project mainland-compatible security enforcement into a jurisdiction with a structurally different legal tradition.
% TRANSFER_FUNCTION: Moves adjudicative authority and procedural control from Hong Kong's common law judiciary and legal profession to a mainland-administered parallel apparatus (designated judges panel, Office for Safeguarding National Security, NPCSC interpretive supremacy) — transferring the capacity to define legal outcomes in security-flagged cases from common law reasoning to mainland legal-administrative logic, cloaked in the continuity of the same court buildings and the same nominal statute book.
% ABSENT_VOICES: Hong Kong's pre-2020 legal academy and the broader common law bar were not consulted on the NSL's drafting, which occurred in Beijing and was inserted directly into HK's Basic Law Annex III without local legislative process. Foreign-qualified barristers, previously an informal check on procedural quality in complex cases, are structurally excluded from the cases where the transplantation is most visible.
% DISAPPEARANCE_RATIONALE: If the NSL and its parallel jurisdictional apparatus vanished, national security prosecutions would revert entirely to ordinary common law criminal procedure (jury trial, standard bail presumption, unrestricted counsel choice), the NPCSC's interpretive supremacy over HK court rulings would lapse, and the designated-judges mechanism would dissolve — Hong Kong's judiciary would operate as a single-track common law system again, materially changing case outcomes and procedural norms in security-adjacent matters.
% FOUNDING_PROBLEM: Stated founding problem: the absence, prior to 2020, of any HK statute criminalizing secession, subversion, terrorism, and collusion with foreign forces at a level the central government judged adequate after the 2019 protest movement — closing what Beijing characterized as a two-decade-old legislative gap left by the failure of the 2003 Article 23 attempt.
% FOUNDING_PROBLEM_CORROBORATION: Beijing-aligned officials and the HK government attest the gap was real and the law simply fills it. Independent corroboration is mixed: comparative constitutional scholars outside China note HK already possessed sedition, public order, and terrorism-adjacent offenses under existing common law and colonial-era statute, and argue the NSL's distinctive contribution is not criminalizing previously-lawful conduct but rather the parallel jurisdictional and interpretive architecture — the designated-judges system and NPCSC supremacy — which was not necessary to criminalize the named conduct and exists independently of it. This corroboration comes from outside both the mainland security apparatus and the HK government that jointly benefit from the arrangement.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.68 at 2025) because the transplantation captures something narrower and more specific than the full space of civil liberties: institutional and professional independence of the common law judiciary and bar, not the broader political-space closure the sibling democratic_enclosure_reading addresses. Suppression (0.71) reflects that the designated-judges system and NPCSC interpretive finality are actively enforced structural mechanisms, not passive drift — a judge cannot decline designation, a court cannot decline NPCSC interpretive supremacy. Theater ratio (0.4) is moderate: the outward continuity of HK courts, wigs, and common law terminology performs continuity while the interpretive substance shifts underneath — genuine theater but not the dominant mechanism (the interpretive supremacy mechanism itself is substantively operative, not merely performative). Accessibility collapse (0.62) is high but not near-total: ordinary civil and most criminal matters remain fully common law; collapse is concentrated in the security-designated track. Resistance (0.58) reflects sustained bar association objections, departures of foreign judges from the Court of Final Appeal, and international legal commentary — real friction, not capitulation.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland security apparatus and the liaison office sit at the clear beneficiary end: they gain a jurisdictional foothold and interpretive control they did not previously possess inside HK's legal order, with analytical/insulated exit (they are not subject to the mechanism, they administer it). HK judiciary and legal profession sit near the target end: institutional power nominally, but exit is constrained by professional identity, licensing jurisdiction, and the practical impossibility of practicing common law elsewhere while remaining a HK-qualified judge or barrister mid-career. Common law litigants are powerless and trapped — they cannot select their procedural track. Foreign-qualified barristers are excluded rather than extracted from directly; their global mobility means low direct extraction but they function as a removed check, which is why they appear as excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem test separates this reading cleanly from a null hypothesis of 'nothing changed.' If HK already possessed adequate sedition and security statutes (contested by independent comparative scholars, corroboration outside the benefiting parties), then the specific institutional apparatus this reading examines — designated judges, NPCSC supremacy, foreign-counsel exclusion — was not necessary to close the criminalization gap and persists for a different reason: control over legal interpretation and outcome, not substantive criminalization. This is precisely the tangled_rope signature: a genuine coordination function (some security enforcement mechanism was arguably needed) bundled with asymmetric extraction (a parallel jurisdictional architecture that transfers interpretive control) sustained by active enforcement (designation cannot be declined, NPCSC rulings cannot be appealed). Classifying it as pure snare would miss the genuine, if narrower, coordination problem being solved; classifying it as pure rope would ignore that the mechanism concentrates control asymmetrically on an institution (mainland security/liaison apparatus) at the direct cost of another (HK judiciary/bar) through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transplantation_vs_supplementation,
    'Is the NSL''s parallel jurisdictional architecture (designated judges, NPCSC interpretive supremacy) a genuine legal-system transplantation that displaces common law doctrine, or a bounded supplementary mechanism operating alongside an otherwise intact common law system?',
    'Longitudinal tracking of NPCSC interpretive interventions and designated-judges caseload as a share of total HK judicial activity over 10-15 years; comparative analysis of whether interpretive methodology in non-security HK case law shows spillover drift toward mainland statutory-interpretation norms.',
    'If NPCSC interpretive intervention and designated-judges caseload remain narrowly bounded to the security track with no spillover into ordinary common law reasoning, this reading''s extractiveness is overstated and the arrangement is closer to a bounded scaffold; if the mainland-style interpretive methodology visibly migrates into general HK jurisprudence, this reading''s ε is understated relative to the true scope of transplantation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transplantation_vs_supplementation, empirical, 'Whether jurisdictional capture is bounded to the security track or diffusing into general common law practice.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Given the sovereignty_restoration_reading, democratic_enclosure_reading, and this jurisdictional_capture_reading all describe the same NSL text, where precisely does the disagreement between readings live — in the facts about what the law does, or in the normative frame for evaluating institutional change of this kind?',
    'Structural comparison of the three readings'' beneficiary/victim declarations and coordination_function statements: if the readings agree on primary factual mechanisms (designated judges exist, NPCSC has interpretive supremacy, certain offenses are newly codified) but disagree on evaluative framing (restoration vs. capture vs. enclosure), the disagreement is normative/conceptual, not empirical.',
    'If the disagreement is purely normative, no future factual discovery resolves which reading is ''correct'' — the readings coexist permanently as competing legitimate framings, consistent with the coexists_with relation declared in cs_structure. If factual disagreement is found (e.g., disputed claims about pre-2020 statutory adequacy), some empirical resolution is possible for narrow sub-claims even while the overall evaluative frames remain irreducibly plural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s sibling readings diverge on facts or on evaluative framework.').

omega_variable(
    professional_identity_lock_severity,
    'How much of HK judiciary/bar''s apparent constraint compliance reflects genuine professional identity lock (career and licensing dependence making exit unthinkable) versus rational calculation under coercive threat (visible departures already occurring among Court of Final Appeal overseas judges)?',
    'Track departure rates and stated reasons among HK-qualified judges and senior barristers over the interval; distinguish identity-locked stayers (who describe the departure of colleagues as a betrayal of institutional commitment) from calculating stayers (who describe remaining as a temporary economic decision).',
    'High identity-lock severity would mean the payer seats'' effective extraction is understated by exit_options alone (constrained undercounts true entrapment); low identity-lock with high departure rates would suggest the constraint''s suppression is more purely coercive/structural and less internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(professional_identity_lock_severity, empirical, 'Whether HK legal profession''s continued participation reflects identity lock or rational short-term calculation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2022, 0.34).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2021, 0.51).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2022, 0.63).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2023, 0.67).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2024, 0.69).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the single natural-language label 'the National Security Law' (nsl_legal_text kernel), per the ε-invariance principle: measuring the NSL by its effect on judicial/professional independence (this story) yields a different ε and a different victim/beneficiary structure than measuring it by its effect on democratic contestation space (democratic_enclosure_reading, higher and broader extraction, victims centered on activists/press/civil society) or by its effect on public order restoration (sovereignty_restoration_reading, low extraction, beneficiaries the general HK/mainland public). All three share the same underlying statutory text but are not the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
