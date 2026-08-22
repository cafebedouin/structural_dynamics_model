% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Manifesto Suspension Regime — Temporal Accommodation Reading
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   The 1890 Manifesto suspended the practice of plural marriage without
 *   renouncing the doctrine: the revealed text stays in canon, the eternal
 *   principle remains valid, and obedience to the law of the land takes
 *   precedence for the present. This story instantiates the
 *   temporal_accommodation_reading of the eternal_marriage_covenant kernel,
 *   and authors epsilon for the standing suspension arrangement as THAT
 *   reading assesses it — the arrangement under contest is the suspension
 *   regime itself, never the hypothetical restored-practice arrangement the
 *   reading anticipates. The claim/metric gap is deliberate: from this
 *   reading's own lights the arrangement is a legitimate temporary transition
 *   (hence the scaffold claim with a conditional sunset — restoration when
 *   political constraints lift), while the authored metrics describe the
 *   arrangement's actual operation, including the accumulating costs borne by
 *   plural households and covenant-bound believers and the public-private gap
 *   in compliance. The engine measures the divergence; nothing here
 *   reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - institutional_church_leadership: agenda-setter (institutional/constrained) — issued and administers the suspension, retains the text in canon
 *   - rank_and_file_latter_day_saints: beneficiary with real cost-bearing (organized/identity_locked) — receives legal safety and institutional continuity, carries the suspended covenant
 *   - existing_plural_families: primary target (powerless/trapped) — households de-legitimized by the suspension, no seat in the deciding councils
 *   - covenant_bound_believers: primary target (moderate/identity_locked) — bound to obligations they are forbidden to fulfill
 *   - federal_authorities: external enforcer turned satisfied counterparty (institutional/arbitrage) — built the pressure, accepts the compliance
 *   - post_manifesto_resisting_leaders: excluded insiders (powerful/identity_locked) — marginalized defenders of continued practice
 *   - lds_historians: analytical observer — reconstructs the public-private record from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.66).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.55).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Manifesto Suspension Regime — Temporal Accommodation Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'dc399a26-b991-4cca-bf89-79241ed3b66a').
narrative_ontology:cs_kernel_codification('dc399a26-b991-4cca-bf89-79241ed3b66a', fixed_text).
narrative_ontology:cs_authority_grounding('dc399a26-b991-4cca-bf89-79241ed3b66a', lineage).
narrative_ontology:cs_interpretation_layer_present('dc399a26-b991-4cca-bf89-79241ed3b66a').
narrative_ontology:cs_reading_relation('dc399a26-b991-4cca-bf89-79241ed3b66a', eternal_marriage_covenant__immutable_commandment_reading, influences).
narrative_ontology:cs_reading_relation('dc399a26-b991-4cca-bf89-79241ed3b66a', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('dc399a26-b991-4cca-bf89-79241ed3b66a', foundational, civil_obedience_precedence_over_practice).
narrative_ontology:cs_axiom_status(civil_obedience_precedence_over_practice, holdable).
narrative_ontology:cs_axiom_grounding('dc399a26-b991-4cca-bf89-79241ed3b66a', civil_obedience_precedence_over_practice, deontological).
narrative_ontology:cs_axiom('dc399a26-b991-4cca-bf89-79241ed3b66a', foundational, eternal_principle_retained_not_renounced).
narrative_ontology:cs_axiom_status(eternal_principle_retained_not_renounced, holdable).
narrative_ontology:cs_axiom_grounding('dc399a26-b991-4cca-bf89-79241ed3b66a', eternal_principle_retained_not_renounced, theological).
narrative_ontology:cs_reference_frame('dc399a26-b991-4cca-bf89-79241ed3b66a', politically_conditioned_eternal_principle).
narrative_ontology:cs_drift_state('dc399a26-b991-4cca-bf89-79241ed3b66a', post_statehood_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc399a26-b991-4cca-bf89-79241ed3b66a', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, institutional_church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, covenant_bound_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_authorities).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_law_of_land_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, conditional_suspension_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the public declaration ending new plural marriages under imminent corporate dissolution, property forfeiture, and leadership imprisonment. Keeps the revealed text in canon unchanged while instructing members that civil obedience governs practice for the present. Administers the compliance regime: withdraws authorization for new sealings, disciplines officers who perform them, and manages communication with federal officials. Cannot drop the text without rupturing the believer base and cannot resume practice without inviting renewed dissolution — its room to maneuver lies between the two.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, institutional_church_leadership, agenda_setter,
    institutional, generational, constrained, continental).

% Ordinary members receive continued legal safety, intact congregations, and an institution that survives; they also carry the task of holding a revealed principle they are told not to practice, and of explaining to children raised on the covenant why it is set aside. Their social world, salvation framework, and family ties are all inside the institution; leaving means losing all of them at once.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_latter_day_saints, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_latter_day_saints, payer).

% Wives and children of marriages formed before the declaration lose the prospect of legal recognition and social legitimation ever extending to their households. Husbands remain exposed to prosecution for existing relationships. Some households relocate to colony settlements in Mexico or Canada, trading community and economic security for continued practice. None of them sat in the councils where the household's future was decided.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families, payer,
    powerless, generational, trapped, regional).

% Men and women who entered sealings under assurance of their eternal binding now hold obligations they are forbidden to fulfill. The retained text keeps the promise alive but unsatisfiable in this life. Their temple access, community standing, and sense of salvational standing all depend on the institution administering the suspension; exiting means declaring their own covenants void.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, covenant_bound_believers, payer,
    moderate, biographical, identity_locked, regional).

% Congress, the courts, and territorial prosecutors built the pressure regime: dissolution of the corporation, escheatment of property, disfranchisement, incarceration of practitioners. They accept the declaration as the compliance they demanded, suspend prosecutions, support statehood, and later examine officeholders under oath. They keep the option of resuming the pressure if practice reappears.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, federal_authorities, beneficiary).

% Senior apostles who performed or defended sealings after the public declaration, acting under what they understood as continuing authorization. Summoned before the Senate committee examining the seated senator, they testified; two resigned under pressure. Their position — that the principle remained in force — loses institutional standing; they cannot repudiate decades of service, and remaining means silence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, post_manifesto_resisting_leaders, excluded,
    powerful, biographical, identity_locked, regional).

% Scholars working from diaries, council minutes, sealing records, and court files reconstruct what was said privately versus announced publicly. They publish through university presses outside the institution's control and supply the record the other seats argue from.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, lds_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, institutional_church_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a collective-action crisis: scattered individual defiance guaranteed mutual ruin, since every practitioner was prosecutable and all corporate property stood forfeit. Unified compliance preserved the corporate body, its temples, and its members' legal existence.
% TRANSFER_FUNCTION: Moves compliance from the covenant community to the federal state; moves the cost of disruption onto existing plural families and covenant-bound believers; moves institutional continuity and sole authority to define the covenant's meaning to church leadership.
% ABSENT_VOICES: Plural wives had no seat in the councils that decided their households' future; rank-and-file members learned of the decision from a pulpit announcement after negotiation concluded; the apostles who favored continued practice were heard and then marginalized.
% DISAPPEARANCE_RATIONALE: An overnight disappearance forces an immediate choice the arrangement exists to defer: resume practice and face renewed dissolution and prosecution, or repudiate the retained text and rupture the believer base. Either branch reorganizes the institution, the households, and the church-state settlement around the choice.
% FOUNDING_PROBLEM: Existential federal suppression: the Edmunds-Tucker regime dissolved the corporation, seized temple property, disfranchised members, and imprisoned practitioners — the body could not survive continued practice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the federal record itself: return of escheated property (1893), Utah statehood (1896), cessation of prosecutions, and Senate committee acceptance of compliance testimony (1904-07). Historians working from court files and congressional records attest the suppression regime ended while the suspension persisted — no benefiting-party attestation is required to establish it.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends at 0.66 and rises monotonically across the interval: the immediate disruption costs land at suspension, and extraction accumulates as the arrangement outlives its justification — covenant-bound believers hold unfulfillable obligations longer, plural households stay de-legitimized permanently. Suppression follows an intensify-then-decay arc: enforcement machinery builds through the post-declaration discipline campaigns and the Senate investigations (peak 0.66 at t=15), then relaxes once compliance is demonstrated and statehood settles the constitutional question. Theater peaks mid-interval (0.52 at t=10): the public cessation declaration coexisted with leadership-authorized private sealings throughout the 1890s, the widest public-private gap in the record; after the second declaration the private exceptions stop but a residue of performative maintenance remains (the retained-but-unpracticed text taught as eternally valid). Accessibility collapse is moderate (0.45): colony emigration, open defiance, and exit from the tradition all remained partially available, unlike a natural limit. Resistance is substantial (0.55): post-declaration marriages, two apostolic resignations, and the later schismatic movements that carried the rival reading forward. All three series share one time grid (t=0,5,10,15,20,25,30) so no metric row is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data explains why. From the leadership seat the arrangement is transition management it chose under duress and administers faithfully; from the plural-household and covenant-believer seats the same arrangement is abandonment of promises made to them specifically, enforced by the institution that made the promises; from the federal seat it is victory — sovereignty vindicated at minimal further cost. Same structure, different experienced types per seat; the engine derives this from power, exit, and directional position, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits near the beneficiary end: it collects institutional survival, retained property, and undiminished authority over the covenant's meaning. Rank-and-file members derive low d from their beneficiary declaration, but their identity-locked exit and genuine cost-bearing (carrying a suspended eternal obligation) place them well short of full subsidy — the story's clearest mixed-position seat. Existing plural families are trapped targets: the suspension's costs concentrate on them with no viable exit, driving d toward the full-target end. Covenant-bound believers are identity-locked targets: the retained text keeps their obligation alive precisely so it can bind them while being unfulfillable. Federal authorities sit near the beneficiary end with arbitrage-grade exit — they can resume or relax pressure at will and collect compliance either way. Resisting leaders are excluded insiders pushed toward the target end as their position loses institutional standing. Historians hold the analytical seat and feed no directional arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   This is the story's center of gravity. The reading declares a sunset — restoration when political constraints lift — and the founding problem (existential federal suppression) verifiably died: property returned 1893, statehood 1896, prosecutions ceased, the Senate accepted compliance by 1907. The arrangement persisted anyway; practice was never restored, and the second declaration hardened suspension into permanence. The founding_problem_status=dead x world_rearranges mismatch routes this through the zombie/capture flag honestly: what began as a defensible emergency transition ran mandate-less for most of the interval, its costs continuing to fall on households and covenant-holders who were promised the burden was temporary. The scaffold claim preserves the reading's own self-understanding; the temporal data expose the decayed mandate. The classification prevents mislabeling in both directions — this is not pure coordination (its costs outlived its function) and not pure extraction (it solved a real collective-action crisis at formation) — and the piton question (did anyone still profit enough to maintain it?) is answered by the receipt surface: leadership retained definitional authority over the covenant, which is a concentrated maintenance interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'How would the classification of this same suspension arrangement change under the immutable_commandment_reading or the prophetic_override_reading of the same covenant?',
    'Author the sibling stories and compare computed types: the immutable reading should compute harsher (an unconditional law betrayed by administrative suspension), the override reading softer (a legitimate exercise of supersession authority). The disagreement localizes in whether the retained text remains operative — the suspension''s legitimacy, not its facts.',
    'If the override reading computes as rope while this reading computes as a decaying scaffold, the structural difference is the sunset clause: this reading''s anticipated restoration is what makes the arrangement transitional rather than settled. Committer structure: this story is one reading of the eternal_marriage_covenant kernel, not the topic whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame omega: this constraint is the temporal_accommodation_reading of the eternal_marriage_covenant kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    restoration_trigger_ambiguity,
    'What exactly counts as ''political constraints lifting'' — return of property, statehood, cessation of prosecutions, or full normalization of the church''s political standing?',
    'Compare the timeline of federal concessions (property return 1893, statehood 1896, Senate acceptance 1907) against any documented leadership discussion of restoration conditions in council minutes and private correspondence.',
    'If the trigger condition was satisfied by 1907, the arrangement ran mandate-less for the rest of the interval and the scaffold claim decays toward inertial persistence; if constraints plausibly persisted (officeholder oath disputes, Smoot-era scrutiny), the suspension retained justification longer than the mismatch flag assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_trigger_ambiguity, empirical, 'Whether the declared sunset condition of the temporal accommodation was ever actually satisfied.').

omega_variable(
    doctrine_retention_cost_allocation,
    'Does retaining the revealed text while forbidding its practice impose a cost on covenant-bound believers, or sustain them by keeping their covenants meaningful?',
    'Diaries, correspondence, and sermons of sealed members across the interval addressing the suspended principle''s status; compare communities where the text stayed canonical against the schismatic colonies where practice resumed.',
    'If retention burdens (an unfulfillable deferred obligation held open indefinitely), covenant_bound_believers belong firmly among victims and effective extraction rises; if retention sustains, the victim set shrinks toward existing plural families alone and the arrangement''s coordination share grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_retention_cost_allocation, conceptual, 'Whether doctrinal retention allocates cost to or sustains the believers who hold the suspended covenants.').

omega_variable(
    post_declaration_exception_scale,
    'How many sealings were performed with leadership authorization between the public declaration and the second declaration, and does the public-private gap constitute performative maintenance or ordinary transitional noncompliance?',
    'Cross-reference sealing records, apostolic journals, and Senate testimony; adopt scholarly estimates of post-declaration marriage volume with uncertainty bounds.',
    'A large authorized-exception volume raises the mid-interval theater ratio and supports reading the arrangement''s public face as substantially performative; a small volume supports a good-faith-transition reading and lowers the theater trajectory accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_declaration_exception_scale, empirical, 'Scale of leadership-authorized private exceptions behind the public compliance declaration, 1890-1904.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(eter_tr_t0, observed).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(eter_tr_t5, observed).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement_basis(eter_tr_t10, observed).
narrative_ontology:measurement(eter_tr_t15, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(eter_tr_t15, observed).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(eter_tr_t20, observed).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(eter_tr_t25, observed).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(eter_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(eter_be_t0, observed).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(eter_be_t5, observed).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(eter_be_t10, observed).
narrative_ontology:measurement(eter_be_t15, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(eter_be_t15, observed).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(eter_be_t20, observed).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(eter_be_t25, observed).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(eter_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(eter_su_t0, observed).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(eter_su_t5, observed).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(eter_su_t10, observed).
narrative_ontology:measurement(eter_su_t15, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(eter_su_t15, observed).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(eter_su_t20, observed).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(eter_su_t25, observed).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(eter_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 Manifesto' covers three structurally distinct commitments sharing one canonical text (D&C 132): the text as unconditional eternal law (immutable_commandment_reading), the living prophet's authority to supersede it (prophetic_override_reading), and the text as valid but politically suspended (this story). Per the epsilon-invariance principle these are three constraints, not one measured three ways: each gets its own epsilon, victim set, and type, and the family is linked through affects_constraints. This story authors epsilon for the suspension arrangement as the temporal-accommodation reading assesses it; the upstream canonical-text commitment is cited by all three readings as evidence for their own divergent conclusions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
