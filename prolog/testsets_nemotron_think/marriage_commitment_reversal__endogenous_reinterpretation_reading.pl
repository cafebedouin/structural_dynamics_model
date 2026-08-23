% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Polygamy Reversal via Woodruff's 1890 Revelation (Endogenous Reinterpretation Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   In September 1890, LDS Church President Wilford Woodruff issued the
 *   Manifesto (Official Declaration 1) declaring the end of plural marriage,
 *   framing it as divine revelation received in response to changed
 *   circumstances — specifically, that God had shown him the church would be
 *   disincorporated and temples seized if the practice continued. This
 *   reading (endogenous_reinterpretation) treats the reversal as an authentic
 *   exercise of continuing prophetic authority: God's will changed because
 *   circumstances changed, and the living prophet correctly discerned the new
 *   will. The constraint is the institutional commitment to this revelation
 *   narrative as the binding interpretive frame for the marriage-commitment
 *   kernel (Section 132). It coordinates institutional survival and member
 *   loyalty while extracting theological consistency (the 'everlasting
 *   covenant' is now suspended without being revoked) and suppressing
 *   dissident fundamentalist alternatives. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination + asymmetric extraction) while the metrics describe moderate
 *   extractiveness with shifting suppression — the engine computes per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.48).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.55).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Polygamy Reversal via Woodruff's 1890 Revelation (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '322b4ef3-e990-4a7c-8cb3-7e952a72b694').
narrative_ontology:cs_kernel_codification('322b4ef3-e990-4a7c-8cb3-7e952a72b694', formalized).
narrative_ontology:cs_authority_grounding('322b4ef3-e990-4a7c-8cb3-7e952a72b694', lineage).
narrative_ontology:cs_interpretation_layer_present('322b4ef3-e990-4a7c-8cb3-7e952a72b694').
narrative_ontology:cs_reading_relation('322b4ef3-e990-4a7c-8cb3-7e952a72b694', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('322b4ef3-e990-4a7c-8cb3-7e952a72b694', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('322b4ef3-e990-4a7c-8cb3-7e952a72b694', foundational, continuing_revelation_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(continuing_revelation_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('322b4ef3-e990-4a7c-8cb3-7e952a72b694', continuing_revelation_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('322b4ef3-e990-4a7c-8cb3-7e952a72b694', foundational, prophetic_authority_binds_conscience).
narrative_ontology:cs_axiom_status(prophetic_authority_binds_conscience, holdable).
narrative_ontology:cs_axiom_grounding('322b4ef3-e990-4a7c-8cb3-7e952a72b694', prophetic_authority_binds_conscience, theological).
narrative_ontology:cs_reference_frame('322b4ef3-e990-4a7c-8cb3-7e952a72b694', section_132_everlasting_covenant).
narrative_ontology:cs_drift_state('322b4ef3-e990-4a7c-8cb3-7e952a72b694', post_1890_manifesto, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('322b4ef3-e990-4a7c-8cb3-7e952a72b694', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, faithful_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissident_fundamentalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, faithful_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_authority).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_survival_as_divine_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives and declares the revelation; controls the interpretive frame for the entire church. Woodruff (1890), Lorenzo Snow (1898-1901), Joseph F. Smith (1901-1918) each reinforce the revelation narrative. The prophet's authority derives from the revelation itself — a circular legitimation that the constraint maintains. Exit is analytical: the prophet could theoretically receive a new revelation reversing the Manifesto, but the institutional cost would be catastrophic.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet_president, agenda_setter,
    institutional, generational, analytical, universal).

% Accepts the Manifesto as divine will. Gains: continued temple access, reduced persecution, statehood for Utah (1896), institutional stability. Pays: cognitive work reconciling Section 132 ('everlasting covenant') with its suspension; identity-locked exit — leaving means losing family, community, salvation framework, and social capital built over generations. The revelation narrative is rehearsed in General Conference, temple recommends, and curriculum, making dissent internally costly.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, faithful_membership, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, faithful_membership, payer).

% Maintain that Section 132 remains binding and the Manifesto was political expediency, not revelation. Bear excommunication (post-1904 systematic), loss of temple access, property seizures (federal), social ostracization from mainstream community, and legal prosecution. Some form Fundamentalist LDS groups (e.g., Short Creek, Centennial Park). Exit is trapped: they cannot rejoin the mainstream without recanting their core conviction; they cannot fully exit the theological framework without losing their salvation narrative.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissident_fundamentalists, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissident_fundamentalists, excluded).

% Enforce anti-polygamy laws (Edmunds Act 1882, Edmunds-Tucker Act 1887). The Manifesto (1890) and Second Manifesto (1904) are read as compliance signals. They monitor for continued plural marriages (Reed Smoot hearings 1904-1907 test this). They do not adjudicate the revelation's authenticity — only behavioral compliance. Their exit is analytical: policy can shift (and did, from prosecution to accommodation).
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% The abstract principle that a canonized 'everlasting covenant' (D&C 132) cannot be suspended by a non-canonized declaration without formal revocation. The constraint extracts coherence from this principle: Section 132 remains in the canon but its core practice is suspended. The revelation narrative obscures the gap. No agent 'represents' this victim — it is a structural casualty of the arrangement, borne by the kernel itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive frame that allows the institution to survive existential federal threat while maintaining prophetic authority and member cohesion. Solves the coordination problem: how to change a core practice without admitting the prior prophetic authority was wrong or the current authority is illegitimate.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed canonized text (Section 132) to living prophetic office (Woodruff and successors). Theological consistency bears the cost (coherence debt); institutional legitimacy and member retention accrue to leadership. Federal pressure is the external condition but not the transfer mechanism in this reading.
% ABSENT_VOICES: Dissident fundamentalists (excommunicated, marginalized, or exiled to Mexico/Canada); plural wives whose marriages were 'suspended' but not dissolved — their voices absent from the revelation narrative; early critics (e.g., Salt Lake Tribune, federal prosecutors) who read the Manifesto as political theater.
% DISAPPEARANCE_RATIONALE: If the revelation narrative vanished overnight, the institutional legitimacy frame would collapse: either Section 132 resumes as binding practice (triggering federal re-intervention) or the prophetic authority that suspended it is revealed as fallible (triggering succession crisis). The Fundamentalist schism would claim vindication. The 120+ years of theological elaboration (Second Manifesto, 1904+ loyalty tests, correlation program) would lose their anchoring premise.
% FOUNDING_PROBLEM: The LDS Church faced existential threat from the Edmunds-Tucker Act (1887): disincorporation, seizure of all assets over $50,000 (including temples), abolition of women's suffrage in Utah, and removal of church control over marriage. By 1890, the Supreme Court had upheld the Act (Late Corp. v. United States). The church faced institutional death if plural marriage continued.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records (Late Corp. v. United States, 1890), Edmunds-Tucker Act text, congressional testimony (Reed Smoot hearings 1904-1907), and contemporary non-Mormon newspaper accounts (New York Times, Salt Lake Tribune) corroborate the existential threat. The church's own leadership (Woodruff, Cannon, Snow) attested to the threat in private correspondence. Critics (fundamentalists, historians like D. Michael Quinn) argue the threat was manageable and the revelation was preemptive capitulation.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the asymmetric cost distribution: institutional leadership preserves legitimacy and authority (beneficiary), while theological consistency bears the incoherence cost and dissident fundamentalists bear exclusion/excommunication (victims). The coordination function is real — the revelation narrative prevented institutional destruction and provided a unified interpretive frame for the membership. Suppression (0.55) is moderate: early phase (1890-1894) driven by federal pressure; later phase (1904+) driven by internal disciplinary enforcement (Second Manifesto, loyalty tests). Theater ratio (0.38) rises as the revelation narrative requires performative maintenance — public testimonies, lesson manuals, and temple recommend interviews rehearse the revelation's authenticity. Accessibility collapse (0.58) is moderate: for faithful members, the revelation narrative closes the exit to fundamentalist alternatives; for dissidents, alternatives exist but at high social cost. Resistance (0.45) reflects organized fundamentalist schism and internal member questioning.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the constraint is a rope: genuine coordination solving an existential threat via legitimate authority. From the dissident_fundamentalist seat, it is a snare: the revelation is cover for capitulation, and enforcement suppresses the true doctrine. From the faithful_membership seat, it is a tangled_rope: real coordination benefit (survival, reduced persecution) paired with real extraction cost (theological incoherence, identity_locked exit). The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The prophet_president (agenda_setter) sits near the beneficiary end (d ≈ 0.15): the revelation consolidates interpretive authority and institutional survival. Faithful_membership (beneficiary/payer) sits near symmetric (d ≈ 0.45): they gain institutional continuity and reduced persecution but pay cognitive costs (reconciling Section 132 with the Manifesto) and identity_locked exit costs. Dissident_fundamentalists (payer/excluded) sit at the full target end (d ≈ 0.95): they bear excommunication, property loss, and social ostracization for maintaining the prior practice. Federal_authorities (observer) sit at analytical (d = 0.5): they experience the constraint as an external policy outcome. Theological_consistency (excluded, non-agent) is the structural victim — the coherence of the canonized kernel is the extracted resource.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential federal threat to corporate existence) was live in 1890 but arguably dead by 1904 (statehood achieved, Reed Smoot hearings underway). Yet the constraint persists and intensifies theatrically. This is classic mandatrophy: the arrangement outlives its founding problem but the revelation narrative prevents recognition of obsolescence. The classification as tangled_rope (not piton) captures this — the coordination function (institutional unity via living prophet) remains live even as the original threat recedes, but the extraction of theological consistency continues without proportional coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_ambiguity,
    'Was Woodruff''s 1890 revelation a genuine divine communication or a strategic accommodation to federal pressure?',
    'Comparative analysis of Woodruff''s private correspondence, journal entries, and the timing relative to Supreme Court decisions (Late Corp. v. United States, 1890) and the 1890 Manifesto''s political consequences.',
    'If strategic accommodation, the constraint is a snare disguised as revelation; if genuine, it is a tangled_rope with authentic coordination function. The classification hinges on whether the revelation narrative is cover or cause.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_ambiguity, conceptual, 'Whether the revelation narrative is authentic divine communication or constructed cover for political survival.').

omega_variable(
    theological_consistency_longterm_cost,
    'What is the long-term cost to doctrinal coherence when a canonized ''everlasting covenant'' (Section 132) is suspended by a later revelation without formal canonization?',
    'Track schism formation (Fundamentalist LDS groups), retention of dissident populations, and doctrinal elaboration efforts (e.g., 1904 Second Manifesto, 1910+ theological rationalizations) over 50+ years.',
    'High long-term doctrinal instability would increase extractiveness retroactively — the revelation solved an immediate crisis but created a permanent coherence debt. Low instability would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_consistency_longterm_cost, empirical, 'Long-term doctrinal coherence cost of suspending a canonized principle via non-canonized revelation.').

omega_variable(
    kernel_reading_identity,
    'Does this reading (endogenous_reinterpretation) foreclose, coexist with, or influence the exogenous_override_reading and practice_doctrine_gap readings of the same kernel?',
    'Analyze whether institutional actors can simultaneously hold this reading and a sibling reading without logical contradiction. Test: Do official teachings affirm continuing revelation as the exclusive mechanism (forecloses external pressure as cause), or do they acknowledge pressure while centering revelation (coexists)?',
    'If forecloses: this reading claims exclusive legitimacy for the reversal. If coexists: the kernel sustains multiple live readings. If influences: this reading shapes the operating environment for siblings (e.g., makes exogenous_override_reading a ''critic''s reading'' rather than an internal option).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between this kernel reading and its siblings.').

omega_variable(
    suppression_mechanism_shift,
    'Did suppression shift from external (federal) to internal (ecclesiastical discipline, loyalty tests) over the interval, and is the internal suppression structural or internalized?',
    'Track excommunication rates, loyalty oath requirements (e.g., 1904+ temple recommend questions), and post-exit trajectories of dissident fundamentalists — whether suppression persists after leaving the institution.',
    'If internalized suppression dominates, the constraint''s effective suppression is higher than structural measures suggest — the revelation narrative becomes a cognitive constraint carried by members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_shift, empirical, 'Whether suppression migrated from external coercion to internalized doctrinal enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_eri_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(mcr_eri_tr_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.28).
narrative_ontology:measurement(mcr_eri_tr_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1898, 0.33).
narrative_ontology:measurement(mcr_eri_tr_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.38).
narrative_ontology:measurement(mcr_eri_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.38).

% Extraction over time
narrative_ontology:measurement(mcr_eri_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(mcr_eri_be_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.42).
narrative_ontology:measurement(mcr_eri_be_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1898, 0.45).
narrative_ontology:measurement(mcr_eri_be_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.48).
narrative_ontology:measurement(mcr_eri_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mcr_eri_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(mcr_eri_su_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.65).
narrative_ontology:measurement(mcr_eri_su_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1898, 0.55).
narrative_ontology:measurement(mcr_eri_su_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.5).
narrative_ontology:measurement(mcr_eri_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This reading and exogenous_override_reading decompose the same historical event (1890 Manifesto) into structurally distinct constraints: this one centers revelation as cause (ε=0.48, tangled_rope), the other centers coercion as cause (ε higher, snare). The practice_doctrine_gap reading captures the persistent ambiguity (Section 132 canonized but suspended). All three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
