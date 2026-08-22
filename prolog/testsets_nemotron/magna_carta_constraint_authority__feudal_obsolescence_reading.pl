% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Constraint Authority — Feudal Obsolescence Reading
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story models the feudal obsolescence reading of Magna Carta as a
 *   constraint on modern constitutional actors: the claim that the 1215
 *   charter was a baronial compact addressing feudal grievances (wardship,
 *   marriage, scutage, forest law) with no binding authority over modern
 *   sovereignty structures. The reading functions as a piton — a former
 *   restraint (the charter once bound the Crown) whose primary function has
 *   atrophied, but which persists due to institutional inertia and theatrical
 *   maintenance. The charter is ceremonially venerated (Runnymede
 *   anniversaries, British Library displays, school curricula) while being
 *   operationally neutralized. The engine computes per-seat classifications
 *   from the structural data; the claimed_type (piton) and metrics are
 *   authored independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.55).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Constraint Authority — Feudal Obsolescence Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'f6dfc61c-1055-4a62-a568-9cc6430cc590').
narrative_ontology:cs_kernel_codification('f6dfc61c-1055-4a62-a568-9cc6430cc590', fixed_text).
narrative_ontology:cs_authority_grounding('f6dfc61c-1055-4a62-a568-9cc6430cc590', lineage).
narrative_ontology:cs_interpretation_layer_present('f6dfc61c-1055-4a62-a568-9cc6430cc590').
narrative_ontology:cs_reading_relation('f6dfc61c-1055-4a62-a568-9cc6430cc590', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6dfc61c-1055-4a62-a568-9cc6430cc590', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('f6dfc61c-1055-4a62-a568-9cc6430cc590', foundational, charter_constraints_historically_satisfied).
narrative_ontology:cs_axiom_status(charter_constraints_historically_satisfied, holdable).
narrative_ontology:cs_axiom_grounding('f6dfc61c-1055-4a62-a568-9cc6430cc590', charter_constraints_historically_satisfied, conventional).
narrative_ontology:cs_axiom('f6dfc61c-1055-4a62-a568-9cc6430cc590', foundational, parliamentary_supremacy_absorbs_all_prior_restraints).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_absorbs_all_prior_restraints, holdable).
narrative_ontology:cs_axiom_grounding('f6dfc61c-1055-4a62-a568-9cc6430cc590', parliamentary_supremacy_absorbs_all_prior_restraints, conventional).
narrative_ontology:cs_reference_frame('f6dfc61c-1055-4a62-a568-9cc6430cc590', glorious_revolution_settlement).
narrative_ontology:cs_drift_state('f6dfc61c-1055-4a62-a568-9cc6430cc590', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6dfc61c-1055-4a62-a568-9cc6430cc590', '2026-08-15T14:23:11Z').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_holders).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_flexibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government ministers and prime ministers who benefit from the reading that Magna Carta imposes no binding restraint on modern executive action. They invoke the charter ceremonially while treating its clauses as historically superseded, maximizing discretionary power in security, emergency, and administrative domains. The reading costs them nothing and legitimates broad executive latitude.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_holders, beneficiary,
    institutional, biographical, arbitrage, national).

% Parliamentarians, constitutional lawyers, and officials who benefit from the reading that Magna Carta's authority was fully absorbed into and is now exercisable only through parliamentary statute. They treat the charter as a historical precursor whose constraints Parliament can modify or repeal at will, preserving legislative supremacy against judicial or popular constitutionalist challenges.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Civil society groups, legal scholars, and reform movements that invoke Magna Carta as a living constraint on arbitrary power — habeas corpus, due process, no taxation without representation. They bear the cost of the obsolescence reading: their invocations are dismissed as sentimental or anachronistic, their litigation fails on standing or justiciability grounds, and their political claims are treated as having no constitutional force. Exit requires either constitutional amendment (prohibitive) or judicial revolution (unpredictable).
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, generational, constrained, national).

% Judges, barristers, and legal academics who argue that Magna Carta established juridical principles (lawful judgment, proportionality, access to courts) that bind the Crown and its successors as a matter of common law inheritance, not mere statute. They bear the cost of the obsolescence reading: their judgments are narrowed or overruled by higher courts citing parliamentary sovereignty, their academic arguments are marginalized, and their professional credibility is staked on a reading the institutional establishment treats as obsolete.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents, payer,
    organized, generational, constrained, national).

% Public officials, educators, and heritage institutions that perform reverence for Magna Carta at anniversaries and in curricula while endorsing the obsolescence reading in practice. They are excluded from the structural contest because their role is to maintain the charter's symbolic capital without demanding its operational enforcement — they legitimize the arrangement by ritualizing it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, ceremonial_constitutionalists, excluded,
    moderate, biographical, mobile, national).

% Scholars of constitutional history and comparative law who analyze the Magna Carta's reception across jurisdictions (US, Canada, Australia, India, etc.) where it is treated as binding precedent. They observe the UK's distinctive obsolescence reading from outside, documenting how the same text functions as active constraint elsewhere while being domestically neutralized.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, comparative_constitutional_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The obsolescence reading coordinates institutional stability by settling the constitutional status of a contested founding document: it tells every actor that the charter is a museum piece, not a litigable instrument, preventing constitutional crises over 800-year-old clauses.
% TRANSFER_FUNCTION: Moves constitutional authority from the charter's text and the popular/juridical actors who invoke it, to the executive and parliamentary actors who control the modern state's discretionary powers. The transfer is legitimacy: the reading converts a potential constraint into a legitimating symbol for the very powers it once restrained.
% ABSENT_VOICES: The 1215 barons themselves — the original parties to the compact — are structurally absent. So are the colonized peoples whose subjection was justified by the same imperial sovereignty the obsolescence reading defends. The US founders, who treated Magna Carta as binding precedent, are excluded from the UK's domestic constitutional conversation. Their presence would challenge the reading's claim that obsolescence is a natural historical progression rather than a jurisdictional choice.
% DISAPPEARANCE_RATIONALE: If the obsolescence reading vanished overnight — if Magna Carta were suddenly treated as binding precedent in UK courts — executive detention powers, surveillance authorizations, prerogative deployments, and parliamentary repeal of rights statutes would all face immediate juridical challenge. The UK's unwritten constitution would rearrange around a newly activated textual anchor. The reading's persistence is what prevents that rearrangement.
% FOUNDING_PROBLEM: The post-Glorious Revolution settlement needed to reconcile parliamentary supremacy with the continued existence of a charter that explicitly limited royal power. The obsolescence reading solved this by declaring the charter's constraints historically spent — satisfied by the Revolution itself — so that Parliament could claim the charter's legacy while retaining unlimited legislative power.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (reconciling parliamentary supremacy with a limiting charter) is attested as dead by the parliamentary sovereignty tradition itself — Dicey, Jennings, and modern UK constitutional lawyers confirm the settlement is complete. But the corroboration comes from the beneficiary tradition. No independent corroboration exists: popular constitutionalists (Allan, Bradley), juridical restraint scholars (Laws LJ, Sedley LJ), and comparative observers (Amar, Sunstein) all contest that the problem was ever resolved rather than suppressed. The obsolescence reading's own tradition is the sole attestor of resolution.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is moderate-to-high because the reading extracts constitutional legitimacy from popular and juridical actors who would invoke the charter as binding restraint — their invocations are systematically deflected, their litigation fails, their political claims are marginalized. The extraction is not rent in the monetary sense but legitimacy-rent: the reading converts a potential constraint into a legitimating symbol for the powers it once restrained. Theater ratio (0.62) is high: ceremonial veneration (anniversaries, displays, curricula) far exceeds operational enforcement. The charter's clauses are not enforced; they are performed. Suppression (0.55) is moderate: the reading does not criminalize dissent but structurally excludes it through justiciability doctrines, standing rules, and the orthodox dismissal of charter-based arguments. Accessibility collapse (0.35) is low: alternatives (living constitutionalism, parliamentary sovereignty with judicial review) remain intellectually and politically available. Resistance (0.45) is moderate: sustained academic, judicial, and civil society pushback exists but has not shifted the institutional consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the executive/parliamentary seat, the arrangement is a settled constitutional order — the charter's work is done, its clauses are spent, and invoking it today is category error. From the popular/juridical seat, the same arrangement is an active suppression of constitutional memory — the charter's principles (habeas corpus, due process, no taxation without representation) are living restraints that the obsolescence reading neutralizes to protect executive discretion. The engine computes this divergence from the structural data; the claimed piton type captures the atrophied-restraint dynamic from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive discretion holders and parliamentary sovereignty advocates are beneficiaries (d ~ 0.15-0.25): the reading costs them nothing and expands their operational latitude. Popular constitutionalism advocates and juridical restraint proponents are payers (d ~ 0.75-0.85): they bear the legitimacy costs of having their constitutional invocations treated as anachronistic. Ceremonial constitutionalists are excluded (d ~ 0.5): they perform the charter's symbolism without demanding its enforcement, structurally legitimating the arrangement. Comparative observers sit at analytical (d = 0.5): they observe the divergence between UK and Commonwealth receptions without being subject to either.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling parliamentary supremacy with a limiting charter) is dead per the beneficiary tradition but contested per the payer tradition. The mismatch (founding_problem_status = dead + disappearance_verdict = world_rearranges) flags this as a mandate that has outlived its function but persists through theatrical maintenance — the piton signature. The reading prevents mislabeling coordination as pure extraction: it genuinely coordinates institutional stability (settling the charter's status) while extracting legitimacy from those who would activate the charter. The extraction is the price of the coordination, not its whole purpose — hence tangled_rope dynamics within a piton trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_legitimacy,
    'Is the feudal obsolescence reading a genuine historical judgment about the charter''s 1215 scope, or a strategic reading adopted to maximize executive discretion in the modern period?',
    'Comparative analysis of 17th-18th century legal discourse: did Coke, Selden, and the Whig historians treat Magna Carta as feudal-obsolescent or as living precedent? If the obsolescence reading emerges only after parliamentary supremacy is secured (post-1689), it is strategic; if it has earlier roots, it may be historical.',
    'If strategic, the reading is a snare (obsolescence claimed to enable extraction) rather than a piton (atrophied restraint). If historical, the piton classification holds — the restraint genuinely atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_legitimacy, conceptual, 'Whether the reading''s origin is historical discovery or strategic construction.').

omega_variable(
    extraction_measurement_ambiguity,
    'How to measure ''extractiveness'' for a constraint whose extraction is legitimacy-rent rather than material transfer?',
    'Develop a legitimacy-extraction metric: track the frequency and success rate of Magna Carta invocations in UK courts vs. Commonwealth courts over time. The divergence quantifies the extraction.',
    'If UK invocation success rate trends to zero while Commonwealth rates remain substantial, the extraction is measurable and the 0.68 epsilon is grounded. If both trend together, the extraction may be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_measurement_ambiguity, empirical, 'Measurement validity for non-material extraction in constitutional constraints.').

omega_variable(
    piton_vs_snare_boundary,
    'Is the constraint a piton (atrophied restraint maintained by inertia/theater) or a snare (active extraction disguised as obsolescence)?',
    'Test whether the reading''s beneficiaries actively defend it against challengers (snare) or merely neglect it (piton). Track government responses to charter-based litigation: active opposition (interventions, appeals, legislative overrides) = snare; passive dismissal = piton.',
    'Snare classification would require active enforcement flag and higher suppression; piton classification fits current metrics (no active enforcement, moderate suppression, high theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_boundary, conceptual, 'Whether the constraint''s persistence is passive (inertia) or active (defended extraction).').

omega_variable(
    commonwealth_counterfactual,
    'Does the charter''s active constraint status in the US, Canada, Australia, and India prove the UK''s obsolescence is a jurisdictional choice rather than historical necessity?',
    'Comparative constitutional history: trace when and why each jurisdiction diverged. If divergence correlates with written constitution adoption (US 1789, Canada 1867/1982, Australia 1901, India 1950) rather than historical distance from 1215, the UK''s unwritten constitution is the variable — the obsolescence reading is a choice enabled by constitutional form.',
    'If jurisdictional choice, the obsolescence reading is more extractive (a chosen neutralization, not an inevitable fading). The epsilon may be understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commonwealth_counterfactual, empirical, 'Whether the UK''s obsolescence reading is historically determined or constitutionally contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1689, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1689, 0.15).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1765, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1765, 0.22).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1832, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1832, 0.35).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1911, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1911, 0.48).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1972, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1972, 0.55).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t2005, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2005, 0.59).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1689, 0.25).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1765, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1765, 0.35).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1832, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1832, 0.42).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1911, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1911, 0.55).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1972, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1972, 0.62).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t2005, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1689, 0.3).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1765, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1765, 0.35).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1832, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1832, 0.4).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1911, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1911, 0.48).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1972, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1972, 0.52).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t2005, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliamentary_sovereignty_doctrine).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_executive_prerogative_powers).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_human_rights_act_1998_constraint).

% DUAL FORMULATION NOTE:
% This constraint family (magna_carta_constraint_authority) decomposes the natural-language concept 'Magna Carta's constitutional authority' into three structurally distinct readings with different ε, beneficiaries, victims, and types. The obsolescence reading (this file) has the highest extractiveness and the most institutional beneficiaries. The living constitutionalism reading has lower extractiveness but active juridical beneficiaries. The parliamentary sovereignty reading sits between — it absorbs the charter into statute, creating a different coordination/extraction balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, institutional, 0.18).
constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, organized, 0.82).
constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, moderate, 0.5).
constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
