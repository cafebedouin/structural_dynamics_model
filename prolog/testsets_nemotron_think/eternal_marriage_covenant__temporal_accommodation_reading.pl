% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Woodruff Manifesto Temporary Suspension of Polygamous Practice
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   The 1890 Woodruff Manifesto officially suspended the practice of plural
 *   marriage in the LDS Church under intense federal pressure (Edmunds-Tucker
 *   Act, disincorporation, disfranchisement). The Manifesto declared
 *   compliance with the law of the land while explicitly not renouncing the
 *   doctrinal principle of eternal marriage covenant including plurality.
 *   This reading holds that the suspension is temporary — the eternal
 *   principle remains valid but dormant, awaiting restoration when political
 *   constraints lift. Federal pressure created the suspension; the doctrine
 *   remains intact but unenforced. The constraint is the institutional
 *   arrangement that maintains doctrinal validity while enforcing behavioral
 *   compliance with civil law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.75).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Woodruff Manifesto Temporary Suspension of Polygamous Practice").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '8a9488bf-e977-434d-86a9-9858656af82b').
narrative_ontology:cs_kernel_codification('8a9488bf-e977-434d-86a9-9858656af82b', fixed_text).
narrative_ontology:cs_authority_grounding('8a9488bf-e977-434d-86a9-9858656af82b', lineage).
narrative_ontology:cs_interpretation_layer_present('8a9488bf-e977-434d-86a9-9858656af82b').
narrative_ontology:cs_reading_relation('8a9488bf-e977-434d-86a9-9858656af82b', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a9488bf-e977-434d-86a9-9858656af82b', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('8a9488bf-e977-434d-86a9-9858656af82b', foundational, manifesto_suspends_practice_not_doctrine).
narrative_ontology:cs_axiom_status(manifesto_suspends_practice_not_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('8a9488bf-e977-434d-86a9-9858656af82b', manifesto_suspends_practice_not_doctrine, conventional).
narrative_ontology:cs_axiom('8a9488bf-e977-434d-86a9-9858656af82b', foundational, eternal_principle_awaits_restoration).
narrative_ontology:cs_axiom_status(eternal_principle_awaits_restoration, holdable).
narrative_ontology:cs_axiom_grounding('8a9488bf-e977-434d-86a9-9858656af82b', eternal_principle_awaits_restoration, conventional).
narrative_ontology:cs_reference_frame('8a9488bf-e977-434d-86a9-9858656af82b', post_manifesto_accommodation_frame).
narrative_ontology:cs_drift_state('8a9488bf-e977-434d-86a9-9858656af82b', post_second_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a9488bf-e977-434d-86a9-9858656af82b', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, monogamous_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_civil_law).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, church_survival_principle).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, continuing_revelation_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto and 1904 Second Manifesto under federal threat of disincorporation and asset seizure. Gains survival, Utah statehood, legal legitimacy, and eventual mainstream acceptance. Controls the doctrinal interpretation apparatus (First Presidency, Quorum of Twelve) that maintains the suspension while preserving the eternal principle. Can arbitrate between compliance and resistance by managing the 'dormant doctrine' narrative.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institution, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_institution, beneficiary).

% Experience reduced federal persecution, social stigma, and economic discrimination after the Manifesto. Gain full citizenship rights (voting, office-holding) and mainstream social integration. Constrained exit: leaving the church means losing community, family networks, and eternal marriage sealing for themselves. Their situation improves materially but they must accept the doctrinal dormancy.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, monogamous_members, beneficiary,
    organized, biographical, constrained, national).

% Forced to 'live the principle' covertly or dissolve plural households. Face excommunication if discovered practicing post-Manifesto plural marriage. Suffer economic loss (property seizure), legal jeopardy (cohabitation prosecutions), family separation, and social ostracism. Constrained exit: can join fundamentalist schismatics (losing mainstream standing) or submit to monogamy (violating covenant). Identity-locked for many — plural marriage was constitutive of their exaltation theology.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_families, payer,
    moderate, biographical, constrained, local).

% Reject the Manifesto as illegitimate coercion. Continue plural marriage practice, forming schismatic groups (FLDS, etc.). Face excommunication, complete social exclusion from mainstream church, and ongoing federal/state prosecution. Identity-locked: their self-concept and communal identity are fused with 'living the principle' — exit means losing their entire worldview and community. They are excluded from the institutional conversation but bear the highest costs.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters, excluded).

% External enforcer whose pressure (Edmunds Act, Edmunds-Tucker Act, disfranchisement, Reed Smoot hearings) created the suppression that shaped the Manifesto. Not a stakeholder within the church's commitment system — does not collect from the constraint, nor bear its costs. Its role is the source of the structural suppression, not a seat inside it.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Analyze the Manifesto as a case study in religious accommodation, church-state relations, and doctrinal change. No material stake in the constraint's operation. Provide the analytical seat from which the kernel's three readings are distinguished.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, scholars_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the existential coordination problem of church survival under federal annihilation pressure: by suspending the practice that triggered legal destruction, the institution preserves its corporate existence, achieves statehood, and maintains the doctrinal framework for future restoration. Coordinates member behavior (compliance) with institutional survival.
% TRANSFER_FUNCTION: Moves the costs of federal compliance (dissolution of plural families, abandonment of covenant marriages, social stigma, economic loss) from the church institution onto polygamous families and fundamentalist dissenters. The institution transfers its existential risk onto its most committed members while retaining the doctrinal asset (eternal principle) for future deployment.
% ABSENT_VOICES: Polygamous wives and children had no formal voice in the Manifesto process — their covenant marriages were dissolved by male priesthood leadership under external pressure. Women's organizations (Relief Society) supported the Manifesto publicly but internal dissent was unrecorded. Fundamentalist dissenters were excluded from the institutional conversation by design — their exclusion is what the enforcement machinery maintains.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished overnight, the church would face immediate federal re-imposition of Edmunds-Tucker penalties (disincorporation, asset seizure). Polygamous families would resume open practice. Fundamentalist groups would claim vindication. The mainstream church would lose its legal legitimacy and mainstream social integration. The religious landscape would reorganize around open plural marriage vs. monogamous schism.
% FOUNDING_PROBLEM: Federal legislative and judicial assault (1862 Morrill Act through 1887 Edmunds-Tucker Act) threatened the LDS Church's corporate existence: disincorporation, seizure of all assets over $50,000, disfranchisement of polygamists, replacement of local courts with federal appointees. The church faced institutional death if it continued plural marriage practice openly.
% FOUNDING_PROBLEM_CORROBORATION: The federal threat was substantially resolved by 1896 (Utah statehood) and 1907 (Reed Smoot seated). Non-beneficiary corroboration: U.S. government records confirm the legal threats ceased; independent historians (e.g., Sarah Barringer Gordon, Kathleen Flake) document that the existential federal pressure ended decades before the church formally abandoned the restoration narrative. The church's own 1904 Second Manifesto acknowledged the changed conditions but reaffirmed doctrinal dormancy rather than sunset.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the real costs borne by polygamous families (forced separation, economic loss, legal jeopardy) and fundamentalist dissenters (excommunication, schism). Suppression (0.75) is high because the constraint's persistence depends on active federal enforcement AND internal disciplinary machinery (excommunication, temple recommend denial). Theater ratio (0.45) is moderate: the Manifesto performs compliance for federal authorities while the doctrinal superstructure remains intact, creating a gap between public performance and private belief. The measurement series shows extractiveness peaking at the Second Manifesto (1904) when enforcement intensified, then declining slightly as accommodation normalized. Suppression requirement declines over the interval as federal pressure eases after statehood (1896) and Reed Smoot hearings (1904-1907).
 *
 * PERSPECTIVAL GAP:
 *   From the church institution's seat, the Manifesto is genuine coordination: it solves the existential threat of disincorporation while preserving doctrinal integrity for future restoration. From polygamous families' seat, the same structure operates as enforced extraction: their covenant marriages are dissolved by institutional fiat under external coercion. From fundamentalist dissenters' seat, it is a snare: the coordination story (obedience to law) is cover for capitulation. The engine computes this seat divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institution (agenda_setter) is the structural beneficiary: it gains survival, statehood, and legitimacy (d near beneficiary end). Monogamous members are beneficiaries: reduced persecution, social integration (d low). Polygamous families are primary payers: bear the full costs of dissolution, social stigma, economic disruption (d near target end). Fundamentalist dissenters are payers/excluded: they reject the suspension, face excommunication, and form schismatic groups (d highest). Federal government is an external enforcer, not a stakeholder within the constraint — its pressure creates the suppression but it does not sit inside the church's commitment system. The engine derives d from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal destruction of the church) was live in 1890. By 1920, statehood was secured, the church was legally recognized, and the existential threat had substantially receded. Yet the suspension persisted and doctrinal dormancy became the new steady state. The mandate (survival via compliance) outlived its founding problem, but the constraint did not sunset — it became the permanent arrangement. This is a scaffold that failed to sunset, drifting toward piton: the coordination function (survival) is achieved, but the constraint remains maintained theatrically (doctrinal dormancy) without the declared transition completing. The mandatrophy_resolved flag should be false — the mandate persists without its founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint one reading of the eternal_marriage_covenant kernel, and does the temporal_accommodation_reading''s structural delta (federal pressure creates temporary suspension; doctrine remains dormant pending future restoration) accurately capture its distinction from sibling readings?',
    'Cross-reading structural comparison: map each reading''s beneficiary/victim structure, claimed_type, and drift_state to confirm they instantiate distinct constraints with different ε values.',
    'If readings share ε and structural data, they are not distinct constraints; the kernel decomposition fails. If distinct, each reading gets its own classification and the family linkage via network.affects_constraints is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Commitment that this JSON instantiates exactly one kernel reading per ε-invariance principle.').

omega_variable(
    temporary_vs_permanent_suspension,
    'Is the Manifesto''s suspension genuinely temporary (conditional on political constraints lifting) or a permanent capitulation disguised as temporary?',
    'Historical analysis of leadership statements 1890-1904 (Second Manifesto), internal correspondence, and the trajectory of enforcement against post-Manifesto plural marriages. If enforcement intensified rather than awaiting restoration, the sunset clause is performative.',
    'If permanent capitulation, claimed_type shifts from scaffold to piton (degraded coordination maintained theatrically) or snare (extraction of compliance without coordination benefit). The has_sunset_clause flag would be false in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporary_vs_permanent_suspension, empirical, 'Whether the sunset condition is operational or rhetorical.').

omega_variable(
    suppression_mechanism_federal_vs_internalized,
    'Is the suppression of polygamous practice primarily structural (federal legal enforcement, disfranchisement, asset seizure) or internalized (members adopting monogamy as normative, identity fusion with law-abiding citizenship)?',
    'Post-1890 suppression trajectory: if suppression persists after federal pressure eases (post-statehood, post-Reed Smoot hearings), reclassify as partially internalized. Track fundamentalist schism formation as evidence of unresolved internalization.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s logic persists in member self-conception after external enforcement relaxes. This affects theater_ratio and piton detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_federal_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the suspension''s maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(eter_tr_t1895, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1895, 0.35).
narrative_ontology:measurement(eter_tr_t1900, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1904, 0.45).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1910, 0.48).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.45).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(eter_be_t1895, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1895, 0.6).
narrative_ontology:measurement(eter_be_t1900, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1904, 0.68).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1910, 0.63).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(eter_su_t1895, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1895, 0.78).
narrative_ontology:measurement(eter_su_t1900, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1904, 0.72).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% Eternal marriage covenant kernel decomposes into three constraint stories: (1) immutable_commandment_reading — practice required, Mountain/Tangled Rope profile (high extraction from dissenters, low from faithful); (2) prophetic_override_reading — doctrine changed by living authority, Scaffold/Rope profile (coordination via continuing revelation); (3) temporal_accommodation_reading (this story) — practice suspended, doctrine dormant, Scaffold profile with conditional sunset. Each has distinct ε, beneficiaries, and drift states. Linked via affects_constraints for family contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, institutional, 0.15).
constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, organized, 0.3).
constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, moderate, 0.85).
constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
