% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin — Conceptual Thinkability Reading
 *   domain: monetary_history/institutional_economics/technology_studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'digital money
 *   origin' kernel: that digital money's origin is properly located at the
 *   moment the concept became technically and institutionally conceivable —
 *   roughly the mid-1950s through the 1980s, when cryptographic and
 *   distributed-systems theorists, together with central bank research
 *   departments, first articulated the conditions under which value could be
 *   represented as pure transferable information, well before ordinary
 *   individuals held digital instruments or regulators formally recognized
 *   them. This is a different constraint from the sibling readings that
 *   locate origin at first practical holding or at formal regulatory
 *   recognition — those are separate stories with their own ε values, not
 *   alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - early_computer_science_theorists: sets conceptual vocabulary (institutional/analytical) — agenda_setter
 *   - central_bank_research_departments: internal feasibility framing (institutional/arbitrage) — agenda_setter/beneficiary
 *   - electronic_funds_transfer_architects: translates theory to protocol (organized/arbitrage) — beneficiary
 *   - cash_dependent_populations: bears the retroactive framing cost (powerless/trapped) — payer
 *   - unbanked_communities: structurally absent from the founding moment (powerless/trapped) — excluded
 *   - non_technical_policy_stakeholders: enters after terms are set (moderate/constrained) — excluded
 *   - monetary_historians: adjudicates which origin story counts (analytical/analytical) — observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.42).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.38).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin — Conceptual Thinkability Reading").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/institutional_economics/technology_studies").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'ad7a6851-39ac-4b85-9381-4b302c20cb40').
narrative_ontology:cs_kernel_codification('ad7a6851-39ac-4b85-9381-4b302c20cb40', distributed).
narrative_ontology:cs_authority_grounding('ad7a6851-39ac-4b85-9381-4b302c20cb40', expertise).
narrative_ontology:cs_interpretation_layer_present('ad7a6851-39ac-4b85-9381-4b302c20cb40').
narrative_ontology:cs_reading_relation('ad7a6851-39ac-4b85-9381-4b302c20cb40', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad7a6851-39ac-4b85-9381-4b302c20cb40', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('ad7a6851-39ac-4b85-9381-4b302c20cb40', foundational, conceivability_precedes_and_grounds_instantiation).
narrative_ontology:cs_axiom_status(conceivability_precedes_and_grounds_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('ad7a6851-39ac-4b85-9381-4b302c20cb40', conceivability_precedes_and_grounds_instantiation, conventional).
narrative_ontology:cs_axiom('ad7a6851-39ac-4b85-9381-4b302c20cb40', secondary, technical_articulation_constitutes_historical_origin).
narrative_ontology:cs_axiom_status(technical_articulation_constitutes_historical_origin, holdable).
narrative_ontology:cs_axiom_grounding('ad7a6851-39ac-4b85-9381-4b302c20cb40', technical_articulation_constitutes_historical_origin, conventional).
narrative_ontology:cs_reference_frame('ad7a6851-39ac-4b85-9381-4b302c20cb40', technical_conceivability_as_origin).
narrative_ontology:cs_drift_state('ad7a6851-39ac-4b85-9381-4b302c20cb40', post_digital_currency_proliferation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('ad7a6851-39ac-4b85-9381-4b302c20cb40', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_computer_science_theorists).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_bank_research_departments).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, electronic_funds_transfer_architects).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, cash_dependent_populations).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, unbanked_communities).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, non_technical_policy_stakeholders).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, monetary_instrument_dematerializability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Working in cryptography, distributed systems, and information theory, these researchers articulate the technical conditions under which a monetary instrument could be represented, transferred, and verified as pure information rather than physical token. Their papers and prototypes set the vocabulary that everyone downstream inherits — they do not implement a currency, but they make one thinkable, and thereby set the terms of the entire subsequent debate.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_computer_science_theorists, agenda_setter,
    institutional, generational, analytical, global).

% Internal working groups begin drafting scenario papers and feasibility studies on electronic settlement years before any public rollout. They benefit from being first movers on the conceptual frame — whichever definition of 'digital money' they adopt internally becomes the baseline against which later regulatory recognition is measured. Their exit option is effectively unconstrained: they can shelve, revive, or redefine the concept at will.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_bank_research_departments, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, central_bank_research_departments, beneficiary).

% Engineers building interbank clearing and wire-transfer infrastructure translate the theoretical conceivability into working protocols for institutional actors. They benefit from the conceptual groundwork being credited to their era of implementation, positioning their firms and careers as the origin point of a now-legible field.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, electronic_funds_transfer_architects, beneficiary,
    organized, generational, arbitrage, continental).

% Populations without access to the technical or institutional apparatus in which digital money became thinkable have no voice in defining what counts as the origin of the concept. When the origin story privileges conceptual thinkability among elite technical circles, their much later and much more constrained encounter with digital instruments is retroactively framed as belated adoption rather than a distinct historical moment with its own conditions.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, cash_dependent_populations, payer,
    powerless, biographical, trapped, global).

% Entirely outside the rooms where the conceptual apparatus was assembled, these communities are defined out of the origin narrative before they are ever defined into the monetary system. Their absence from the conceptual founding is not neutral — it means the categories built without them (identity verification, account access, credit history) become naturalized infrastructure they must later adapt to rather than help design.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, unbanked_communities, excluded,
    powerless, generational, trapped, global).

% Legislators, consumer advocates, and labor representatives who might have shaped how digital money's conceptual boundaries were drawn were not present when technologists and central bank researchers set the terms. By the time they enter the conversation, the conceptual frame is already settled and presented to them as a technical fait accompli rather than a live policy choice.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, non_technical_policy_stakeholders, excluded,
    moderate, biographical, constrained, national).

% Retrospectively adjudicate which moment counts as the 'true' origin of digital money — conceptual thinkability, first practical holding, or formal regulatory recognition. Their choice of origin story is not neutral: it allocates historical credit, shapes which institutions are treated as founders, and determines whose exclusion counts as incidental versus structural.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a shared technical and institutional vocabulary for representing monetary value as transferable, verifiable information solves a genuine coordination problem: without a common conceptual frame, independent efforts at electronic settlement, interbank clearing, and account-based value transfer could not interoperate or build on each other.
% TRANSFER_FUNCTION: The arrangement moves interpretive and historical credit — and eventually resource allocation for R&D and infrastructure funding — toward the institutions and individuals present at the conceptual founding, and away from populations and stakeholders who were structurally absent from that founding moment and therefore have no claim on being originators.
% ABSENT_VOICES: Unbanked communities, cash-dependent populations in the Global South, and non-technical policy stakeholders would object that treating conceptual thinkability as the origin moment erases the material and political conditions of monetary access that determine who actually benefits from any 'digital money' regime; they were not in the seminar rooms, working groups, or research departments where the concept was assembled.
% DISAPPEARANCE_RATIONALE: If the 'became thinkable' framing of the origin disappeared, the technical history of cryptography and distributed computing would remain unchanged, but the credit-allocation structure built on top of it — which institutions get called pioneers, which papers get cited as founding documents, which central banks get treated as early movers — would rearrange substantially. Whether the world 'rearranges' depends on whether one is asking about the underlying technical facts (largely fixed) or the institutional narrative built on them (highly mutable).
% FOUNDING_PROBLEM: The founding problem this reading solves is retrospective and interpretive: given that digital money's history has multiple candidate starting points, this reading answers 'when did digital money begin' by locating origin at the moment the concept became technically and institutionally articulable — prior to any practical holding or regulatory recognition.
% FOUNDING_PROBLEM_CORROBORATION: Central bank research departments and computer science historians internal to the technical tradition corroborate that conceptual groundwork substantially preceded implementation. However, economic historians outside the technical tradition (e.g., scholars of monetary sociology and development economics) dispute that conceptual thinkability among a narrow technical elite constitutes a meaningful societal origin point, arguing it privileges producer history over user history — this dispute is the core of the kernel contest itself.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).
:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 by 1985) because the coordination function is genuine — a shared technical vocabulary for value-as-information was a real prerequisite for interoperable electronic settlement — but the credit-allocation and narrative-authority benefits accrue asymmetrically to the technical and institutional actors present at the founding. Suppression is moderate (0.38): there is no coercive enforcement against alternative origin stories, but there is a persistent gatekeeping effect in which the technical framing crowds out competing historical narratives in professional and academic discourse, which requires active maintenance to persist as the operative history taught in economics and technology curricula. Theater ratio is modest (0.28) — most of the underlying technical work was substantive, not performative, though retrospective 'origin story' commemorations by institutions carry a nontrivial performative component.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of central bank research departments and technical theorists, this reading is straightforwardly coordination: solving a genuine representation problem that had to be solved before any implementation could occur. From the seat of unbanked communities and non-technical stakeholders, the same conceptual founding reads as an act of gatekeeping that permanently structured whose needs the eventual system would serve — the engine's per-seat computation should reflect this asymmetry without either side's framing being treated as simply correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Early computer science theorists and central bank research departments are near the beneficiary end: they set the terms of the conceptual frame and collect historical/institutional credit without bearing costs from populations excluded at that stage. Cash-dependent and unbanked populations sit near the full-target end — trapped exit, no voice in the founding moment, and they bear the downstream consequence of infrastructure designed without their participation. Non-technical policy stakeholders sit closer to symmetric-but-excluded: moderate power, but their absence from framing decisions is a structural exclusion rather than active extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (representing monetary value as transferable information) is largely resolved — that technical problem is dead in the sense that the representational question has been settled for decades. What persists is the institutional credit-allocation structure built on top of that resolved problem: continuing to treat 'conceptual thinkability' as the primary origin marker serves the historical status of institutions present at the founding even though the underlying technical problem no longer requires that framing to be maintained. This is exactly the kind of divergence the mandatrophy interview is meant to surface: a founding problem that is dead (technically) but a framing that persists (institutionally) because it allocates credit rather than because it is functionally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_origin_vs_material_origin,
    'Is ''becoming thinkable'' a meaningful historical origin point for a monetary instrument, or is it a category error that privileges producer-side intellectual history over user-side material history?',
    'Comparative historiography: examine whether other monetary innovations (e.g., coinage, banknotes, checks) are conventionally dated to their conceptual articulation or to their point of practical adoption, and whether that convention itself reflects power asymmetries in whose history gets recorded.',
    'If conceptual origin is treated as the canonical answer, credit and historical primacy accrue to technical/institutional elites; if practical adoption or regulatory recognition is treated as canonical, the origin date shifts later and different beneficiary sets are implicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_origin_vs_material_origin, conceptual, 'Whether conceptual thinkability is a defensible historical origin marker or an artifact of who writes the history.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why has the ''became thinkable'' reading been the dominant frame in technology-studies and computer-science historiography specifically, while economic historiography has tended toward the regulatory_recognition_reading?',
    'Discourse analysis of which disciplinary communities cite which origin narrative, and whether citation patterns track institutional affiliation with the beneficiary groups named in each reading.',
    'If the reading selected tracks the author''s disciplinary and institutional position, that is evidence the kernel contest is partly a proxy fight over disciplinary credit rather than a purely factual dispute about origins.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether disciplinary affiliation predicts which sibling reading a given account of digital money''s origin adopts.').

omega_variable(
    false_summit_naturalization_risk,
    'Does treating this reading''s beneficiaries (early technical theorists, central bank researchers) as merely incidental founders of a naturally-emerging concept obscure the degree to which the conceptual frame was actively constructed to serve institutional interests?',
    'Archival review of internal central bank memoranda and research funding decisions from the period to determine whether framing choices were explicitly linked to institutional positioning rather than purely technical necessity.',
    'If framing choices track institutional self-interest, the tangled_rope classification is reinforced; if framing was purely technically determined with no strategic institutional input, the coordination component would dominate and the classification would move toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, empirical, 'Whether the conceptual founding was strategically shaped by institutional interest or emerged from technical necessity alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1955, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1955, digital_money_origin__became_thinkable_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(digi_tr_t1961, digital_money_origin__became_thinkable_reading, theater_ratio, 1961, 0.18).
narrative_ontology:measurement(digi_tr_t1967, digital_money_origin__became_thinkable_reading, theater_ratio, 1967, 0.21).
narrative_ontology:measurement(digi_tr_t1973, digital_money_origin__became_thinkable_reading, theater_ratio, 1973, 0.24).
narrative_ontology:measurement(digi_tr_t1979, digital_money_origin__became_thinkable_reading, theater_ratio, 1979, 0.26).
narrative_ontology:measurement(digi_tr_t1985, digital_money_origin__became_thinkable_reading, theater_ratio, 1985, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1955, digital_money_origin__became_thinkable_reading, base_extractiveness, 1955, 0.22).
narrative_ontology:measurement(digi_be_t1961, digital_money_origin__became_thinkable_reading, base_extractiveness, 1961, 0.28).
narrative_ontology:measurement(digi_be_t1967, digital_money_origin__became_thinkable_reading, base_extractiveness, 1967, 0.33).
narrative_ontology:measurement(digi_be_t1973, digital_money_origin__became_thinkable_reading, base_extractiveness, 1973, 0.37).
narrative_ontology:measurement(digi_be_t1979, digital_money_origin__became_thinkable_reading, base_extractiveness, 1979, 0.4).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__became_thinkable_reading, base_extractiveness, 1985, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1955, digital_money_origin__became_thinkable_reading, suppression_requirement, 1955, 0.2).
narrative_ontology:measurement(digi_su_t1961, digital_money_origin__became_thinkable_reading, suppression_requirement, 1961, 0.25).
narrative_ontology:measurement(digi_su_t1967, digital_money_origin__became_thinkable_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(digi_su_t1973, digital_money_origin__became_thinkable_reading, suppression_requirement, 1973, 0.33).
narrative_ontology:measurement(digi_su_t1979, digital_money_origin__became_thinkable_reading, suppression_requirement, 1979, 0.36).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__became_thinkable_reading, suppression_requirement, 1985, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(digital_money_origin__became_thinkable_reading, 0.05).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'when digital money originated.' The 'became_thinkable' reading (this story) has an earlier origin date and moderate ε (0.42) reflecting diffuse institutional credit-extraction among technical elites. The 'first_held' reading locates origin at individual practical adoption, with a different beneficiary/victim structure (early adopters vs. late adopters) and likely lower institutional concentration. The 'regulatory_recognition' reading locates origin at formal statistical/regulatory incorporation, with higher suppression tied to compliance enforcement and a later origin date. All three are linked via affects_constraints; none should be read as alternative measurements of a single ε — they are structurally distinct constraints sharing a common natural-language label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
