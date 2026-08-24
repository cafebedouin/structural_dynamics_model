% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story models the 'national primacy' reading of Article 17
 *   complementarity under the Rome Statute. Under this reading, national
 *   courts are presumptively adequate to prosecute international crimes; the
 *   ICC may only intervene when the Prosecutor proves proceedings are
 *   'unwilling or unable' genuinely — a high threshold requiring evidence of
 *   sham, collapse, or deliberate shielding. The result: victims in states
 *   with formally existing but substantively weak proceedings (partial
 *   investigations, low-level convictions only, procedural delays protecting
 *   elites) fall outside ICC reach. The coordination function is genuine — a
 *   decentralized system avoiding duplicate prosecutions — but the extraction
 *   is asymmetric: sovereignty-maximizing states and their judiciaries gain
 *   immunity from international scrutiny, while victims in weak-but-genuine
 *   systems bear the cost. The claimed type is tangled_rope (coordination +
 *   asymmetric extraction with active enforcement); the metrics reflect
 *   rising extractiveness as states learn to perform 'genuine' proceedings
 *   that satisfy the formal threshold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.72).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.78).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '6663de24-b1c7-4ba0-af65-2325fdb394a4').
narrative_ontology:cs_kernel_codification('6663de24-b1c7-4ba0-af65-2325fdb394a4', formalized).
narrative_ontology:cs_authority_grounding('6663de24-b1c7-4ba0-af65-2325fdb394a4', lineage).
narrative_ontology:cs_interpretation_layer_present('6663de24-b1c7-4ba0-af65-2325fdb394a4').
narrative_ontology:cs_reading_relation('6663de24-b1c7-4ba0-af65-2325fdb394a4', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('6663de24-b1c7-4ba0-af65-2325fdb394a4', foundational, national_courts_presumptively_adequate).
narrative_ontology:cs_axiom_status(national_courts_presumptively_adequate, holdable).
narrative_ontology:cs_axiom_grounding('6663de24-b1c7-4ba0-af65-2325fdb394a4', national_courts_presumptively_adequate, conventional).
narrative_ontology:cs_axiom('6663de24-b1c7-4ba0-af65-2325fdb394a4', foundational, icc_burden_to_demonstrate_inadmissibility).
narrative_ontology:cs_axiom_status(icc_burden_to_demonstrate_inadmissibility, holdable).
narrative_ontology:cs_axiom_grounding('6663de24-b1c7-4ba0-af65-2325fdb394a4', icc_burden_to_demonstrate_inadmissibility, conventional).
narrative_ontology:cs_reference_frame('6663de24-b1c7-4ba0-af65-2325fdb394a4', rome_statute_complementarity_framework).
narrative_ontology:cs_drift_state('6663de24-b1c7-4ba0-af65-2325fdb394a4', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6663de24-b1c7-4ba0-af65-2325fdb394a4', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_proceedings_states).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, state_sovereignty_primacy_in_criminal_jurisdiction).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, complementarity_as_sovereignty_protection_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer domestic criminal proceedings for international crimes; their adequacy is presumed unless the ICC proves sham. They control evidence, witnesses, and procedural pace. Their decisions are rarely overturned on complementarity grounds. Exit from this role means ceding jurisdiction to an international body they view as illegitimate.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter,
    institutional, generational, constrained, national).

% States that prioritize sovereign control over criminal jurisdiction. They benefit from the high inadmissibility threshold because it shields domestic proceedings — including those protecting political elites — from ICC scrutiny. They can credibly threaten non-cooperation. Their exit option is withdrawal from the Rome Statute, which several have done or threatened.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    powerful, biographical, mobile, national).

% Victims of international crimes in states where domestic proceedings exist but are weak, slow, partial, or protect powerful perpetrators. Because proceedings are not 'sham' — they exist, have some formality, may even convict low-level perpetrators — the ICC finds them admissible and declines jurisdiction. These victims have no access to ICC justice and no realistic exit to another forum.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_proceedings_states, payer,
    powerless, biographical, trapped, national).

% Bears the burden of proving national proceedings are 'unwilling or unable' genuinely to prosecute. Must gather evidence of sham proceedings from within uncooperative states. Resource constraints and state non-cooperation limit investigation. The prosecutor's institutional credibility depends on showing complementarity works, creating pressure to accept national proceedings.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecutor, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, icc_prosecutor, observer).

% Document gaps in national proceedings, advocate for broader 'unwilling or unable' interpretation, represent victims. They participate as amicus curiae but have no formal standing in admissibility challenges. Their evidence of proceeding deficiencies is often discounted if the state demonstrates formal compliance.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, civil_society_ngos, excluded,
    organized, biographical, constrained, global).

% Analyze complementarity jurisprudence, debate 'unwilling or unable' thresholds, critique the presumption of adequacy. Their work influences judicial reasoning over long time horizons but has no direct enforcement power. They see the full structural tension between sovereignty and accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates primary adjudicative authority for international crimes to national courts, presuming their adequacy, thereby coordinating a decentralized enforcement system that respects state sovereignty while maintaining a residual ICC backstop for complete judicial collapse.
% TRANSFER_FUNCTION: Transfers the burden of proving inadmissibility from the state to the ICC Prosecutor; moves jurisdictional authority from the international to the national level in all but the most egregious cases; moves the cost of failed accountability onto victims in states with weak-but-genuine proceedings.
% ABSENT_VOICES: Victims in states with weak-but-genuine proceedings are structurally excluded — their states' proceedings meet the formal threshold for admissibility, so they have no standing to challenge complementarity before the ICC. They would argue for a lower threshold capturing 'ineffective' proceedings, not just 'sham' ones. They are located in the very states whose proceedings block ICC access.
% DISAPPEARANCE_RATIONALE: If the national primacy reading of complementarity vanished, the ICC would exercise jurisdiction whenever national proceedings are ineffective — not only when they are sham. States would lose the presumption of adequacy; the ICC would become a court of concurrent rather than complementary jurisdiction. Sovereignty-maximizing states would likely withdraw en masse. The Rome Statute system would reorganize around either universal ICC jurisdiction or collapse into bilateral immunity agreements.
% FOUNDING_PROBLEM: The 1998 Rome Conference tension: how to create a permanent international criminal court without violating the sovereign equality of states — specifically, how to prevent impunity for the worst crimes while assuring states they would not be subjected to politicized prosecutions by an unaccountable international prosecutor.
% FOUNDING_PROBLEM_CORROBORATION: The Rome Conference negotiating record (UN Doc. A/CONF.183/13) shows states explicitly debated and adopted the 'presumption of adequacy' language to protect sovereignty. However, human rights organizations (Human Rights Watch, Amnesty International) and several state delegations (e.g., Canada, Nordic states) at the time warned this would create an impunity gap for 'weak but genuine' proceedings — a prediction borne out in subsequent jurisprudence (Gaddafi, Kenya, Afghanistan).
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers the cost of accountability gaps onto a specific victim class — those in states that perform procedural compliance without substantive justice. Suppression (0.78) is higher because the constraint's persistence depends on states actively performing 'genuine' proceedings to block ICC access, and on the ICC's institutional incentives to defer. Theater ratio (0.42) reflects that complementarity proceedings are real judicial work, but a growing share serves the performative function of demonstrating 'genuineness' to the ICC rather than delivering justice. Accessibility collapse (0.81) is very high for the victim class — once a state initiates any formal proceeding, the ICC door effectively closes. Resistance (0.55) is moderate: civil society challenges admissibility decisions, but states control the evidence and cooperation needed to overcome the presumption.
 *
 * PERSPECTIVAL GAP:
 *   From the national_judiciaries seat: the system works — they prosecute, the ICC respects sovereignty, complementarity prevents forum shopping. From the victims_in_weak_but_genuine_proceedings_states seat: the system extracts their access to justice — their states' proceedings are a shield, not a sword. From the icc_prosecutor seat: the burden of proof is structurally nearly impossible to meet without state cooperation, creating pressure to accept national proceedings at face value. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are beneficiaries (d near 0.0) — they gain jurisdictional control and immunity from international scrutiny. Victims in weak-but-genuine proceedings are payers (d near 1.0) — they lose ICC access while gaining no effective domestic remedy. The ICC Prosecutor sits near symmetric (d ~0.5) — institutional mandate to prosecute vs. structural inability to overcome the presumption. Civil society NGOs are structurally excluded (exit: constrained) — they can observe and advocate but cannot trigger jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereignty vs. accountability) was live in 1998. Today it is contested: sovereignty-maximizing states claim the problem persists (politicized ICC risk); victims' advocates and some states argue the problem has shifted (impunity via 'genuine' proceedings). The mandate has not been resolved — it has drifted. The national primacy reading prevents mislabeling this as pure coordination (rope) by exposing the victim class that pays for the sovereignty protection, and prevents mislabeling as pure extraction (snare) by acknowledging the real coordination function (decentralized prosecution, avoiding duplicate trials). The tangled_rope classification captures the hybrid: the coordination is real, but the extraction is structural and asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the national primacy reading of Article 17 represent the Rome Statute''s authentic object and purpose, or a strategic reinterpretation by sovereignty-maximizing states?',
    'Comparative analysis of Rome Conference negotiating history, early ICC jurisprudence (Lubanga, Katanga), and subsequent state practice (withdrawal notifications, non-cooperation). The Preparatory Commission and Committee of the Whole records show deliberate ambiguity in ''unwilling or unable'' language.',
    'If the national primacy reading reflects original intent, the victim gap is a known compromise; if it is a strategic drift, the gap represents mandate expansion by states beyond the treaty''s purpose. Affects whether the constraint is a scaffold (transitional) or piton (degraded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the high inadmissibility threshold is original compromise or strategic drift').

omega_variable(
    sham_vs_ineffective_boundary,
    'Where is the structural line between ''sham proceedings'' (inadmissible) and ''weak but genuine proceedings'' (admissible)?',
    'ICC Pre-Trial Chamber jurisprudence mapping: Gaddafi (Libya) — proceedings deemed admissible despite fair trial concerns; Kenya — admissibility challenged but upheld; Afghanistan — investigation authorized after preliminary examination found national proceedings insufficient. Each decision refines the boundary.',
    'If the line is ''any proceeding with formal regularity,'' extraction is maximal (near-snare). If the line requires ''substantive capacity to deliver justice,'' extraction is lower (closer to rope). The victim class size depends entirely on this boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sham_vs_ineffective_boundary, empirical, 'The adjudicative boundary determining which victims fall outside ICC reach').

omega_variable(
    state_cooperation_as_extraction_enabler,
    'Does state non-cooperation with ICC admissibility challenges function as an extraction mechanism — states using sovereignty to withhold evidence that would prove their proceedings are sham?',
    'Analysis of ICC cooperation requests refused on national security or sovereignty grounds in complementarity contexts (e.g., Sudan, Libya, Myanmar). Compare with cooperation in non-complementarity contexts.',
    'If non-cooperation systematically blocks admissibility challenges, suppression is higher than measured — the constraint''s enforcement machinery includes state refusal power. Would support reclassification toward snare from victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_as_extraction_enabler, empirical, 'Whether state non-cooperation is integral to the constraint''s extraction structure').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by victims in weak-but-genuine proceedings structural (legal barriers to ICC access) or internalized (victims believe domestic proceedings are their only path, or fear retaliation for seeking international justice)?',
    'Post-exit suppression trajectory: if victims who secure ICC access (rare) still face community pressure or state retaliation, internalized component is significant. Victim perception surveys in situation countries (DRC, Uganda, CAR, Georgia).',
    'If internalized, effective suppression exceeds the structural measure — victims carry the constraint''s suppression with them even if the legal barrier were removed. Would increase measured extraction for the victim seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the victim class').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__national_primacy_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__national_primacy_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__national_primacy_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(arti_tr_t16, article_17_complementarity__national_primacy_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(arti_tr_t22, article_17_complementarity__national_primacy_reading, theater_ratio, 22, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__national_primacy_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__national_primacy_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__national_primacy_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(arti_be_t16, article_17_complementarity__national_primacy_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(arti_be_t22, article_17_complementarity__national_primacy_reading, base_extractiveness, 22, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__national_primacy_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__national_primacy_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__national_primacy_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(arti_su_t16, article_17_complementarity__national_primacy_reading, suppression_requirement, 16, 0.77).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(arti_su_t22, article_17_complementarity__national_primacy_reading, suppression_requirement, 22, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, icc_state_cooperation_obligations).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, victim_participation_rights_rome_statute).

% DUAL FORMULATION NOTE:
% This constraint and article_17_complementarity__international_oversight_reading form a constraint family decomposing the kernel 'article_17_complementarity'. The national_primacy_reading has higher inadmissibility threshold (ε=0.72) and restricted victim set; the international_oversight_reading has lower threshold, broader victim access, and higher ICC prosecutorial discretion. They share the same Rome Statute text but instantiate different constraints with different ε, beneficiaries, and victims. The upstream constraint (Rome Statute text) influences both; the readings influence each other through judicial dialogue and state practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, powerful, 0.2).
constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
