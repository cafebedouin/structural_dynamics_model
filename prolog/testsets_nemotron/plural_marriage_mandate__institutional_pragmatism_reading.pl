% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Institutional Pragmatism
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) announced the suspension of
 *   plural marriage by LDS Church President Wilford Woodruff, framed as
 *   divine revelation. This reading interprets the Manifesto as strategic
 *   institutional adaptation: the church leadership, facing existential
 *   threat from federal enforcement (Edmunds Act 1882, Edmunds-Tucker Act
 *   1887, disfranchisement, property seizure, leadership imprisonment),
 *   capitulated to superior coercive power while deploying a revelation
 *   narrative to legitimate the capitulation internally. The constraint is
 *   the ongoing mandate of plural marriage as a divine requirement — which
 *   the Manifesto did not doctrinally rescind but practically suspended. The
 *   M-set gap (doctrine unchanged, practice suspended, secret continuations
 *   1890-1904) becomes the primary observable. Church leadership benefits
 *   from institutional survival and restored political rights. Victims
 *   include polygamists coerced into underground practice or abandonment of
 *   families, and monogamists deceived by the public/private divergence.
 *
 * KEY AGENTS:
 *   - church_leadership: Primary beneficiary and agenda_setter (institutional survival, political restoration) — power: institutional / exit: analytical
 *   - coerced_polygamists: Primary victim (abandoned by public doctrine, driven underground, legal jeopardy) — power: powerless / exit: trapped
 *   - deceived_monogamists: Secondary victim (public doctrine / private practice divergence, legitimacy crisis) — power: moderate / exit: constrained
 *   - federal_authorities: External coercive power (not a stakeholder seat; defines the constraint's boundary conditions) — power: institutional / exit: analytical
 *   - dissident_fundamentalists: Excluded voice (reject Manifesto as illegitimate, continue practice openly) — power: organized / exit: identity_locked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.78).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.82).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Institutional Pragmatism").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '6a828829-4b9d-48d9-bf99-20c3c444da9c').
narrative_ontology:cs_kernel_codification('6a828829-4b9d-48d9-bf99-20c3c444da9c', formalized).
narrative_ontology:cs_authority_grounding('6a828829-4b9d-48d9-bf99-20c3c444da9c', extraction).
narrative_ontology:cs_interpretation_layer_present('6a828829-4b9d-48d9-bf99-20c3c444da9c').
narrative_ontology:cs_reading_relation('6a828829-4b9d-48d9-bf99-20c3c444da9c', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('6a828829-4b9d-48d9-bf99-20c3c444da9c', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('6a828829-4b9d-48d9-bf99-20c3c444da9c', foundational, revelation_narrative_legitimates_capitulation).
narrative_ontology:cs_axiom_status(revelation_narrative_legitimates_capitulation, holdable).
narrative_ontology:cs_axiom_grounding('6a828829-4b9d-48d9-bf99-20c3c444da9c', revelation_narrative_legitimates_capitulation, instrumental).
narrative_ontology:cs_axiom('6a828829-4b9d-48d9-bf99-20c3c444da9c', foundational, institutional_survival_outranks_doctrinal_integrity).
narrative_ontology:cs_axiom_status(institutional_survival_outranks_doctrinal_integrity, holdable).
narrative_ontology:cs_axiom_grounding('6a828829-4b9d-48d9-bf99-20c3c444da9c', institutional_survival_outranks_doctrinal_integrity, instrumental).
narrative_ontology:cs_reference_frame('6a828829-4b9d-48d9-bf99-20c3c444da9c', divine_mandate_permanent_binding).
narrative_ontology:cs_drift_state('6a828829-4b9d-48d9-bf99-20c3c444da9c', post_manifesto_1890, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6a828829-4b9d-48d9-bf99-20c3c444da9c', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, mormon_institution).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the Manifesto to preserve the institution from federal destruction. Collected the benefits: property restoration, political rehabilitation (statehood 1896, Smoot seating 1907), narrative control over the revelation frame. Maintained doctrinal authority while practically suspending the practice. Authorized some post-Manifesto plural marriages covertly (1890-1904). Exit is analytical — they interpret the constraint from the seat that administers it.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, beneficiary).

% Bound by covenant to plural marriage as divine requirement. After 1890, faced criminal prosecution, property seizure, exile, or abandonment of families. Some continued covertly with leadership's tacit/quiet authorization (1890-1904); others were abandoned when the Second Manifesto (1904) made new plural marriages excommunicable offenses. No exit: doctrinal identity fuses with the practice; leaving the practice means leaving the salvific framework.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, trapped, local).

% Publicly taught and believed plural marriage was suspended. Privately, leadership authorized continuing plural marriages for select individuals (1890-1904). The public/private divergence created cognitive dissonance and legitimacy crisis when exposed (Smoot hearings). Some benefited from institutional stability; all paid the cost of institutional deception. Exit is constrained — leaving the church severs community, family, and identity.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, beneficiary).

% Rejected the Manifesto as illegitimate — doctrine cannot be suspended by revelation for political expediency. Continued plural marriage openly, forming breakaway groups (FLDS precursors). Their exclusion is structural: the institutional_pragmatism reading treats their position as the dissent that proves the constraint's extraction (they are the ones who refused the capitulation and paid the full price).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, dissident_fundamentalists, excluded,
    organized, generational, identity_locked, regional).

% External coercive power — not a stakeholder seat inside the constraint. Defined the boundary conditions: Edmunds Act (1882), Edmunds-Tucker Act (1887), Supreme Court rulings (Davis v. Beason 1890, Late Corp. of the Church v. U.S. 1890). Their enforcement created the survival threat; their eventual accommodation (statehood, property return) completed the leadership's survival arc. Analytical seat: they observe the constraint's effects from outside.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserved the LDS Church as a viable institution under existential federal coercion — avoiding disincorporation, property seizure, and leadership imprisonment — by strategically suspending the practice that was the primary target of federal enforcement.
% TRANSFER_FUNCTION: Moves institutional survival and political legitimacy from the federal coercive apparatus to the church leadership, at the cost of polygamists' legal security, family integrity, and spiritual standing, and monogamists' epistemic trust in institutional honesty.
% ABSENT_VOICES: Dissident fundamentalists (precursors to FLDS) who rejected the Manifesto as illegitimate capitulation — they were excommunicated, marginalized, and their objection was structurally excluded from the institutional conversation. Polygamist women's voices are largely absent from the leadership's decision calculus — their experience of abandonment, underground pregnancy, and legal jeopardy was not a deliberative input.
% DISAPPEARANCE_RATIONALE: If the plural marriage mandate (and its Manifesto suspension) vanished overnight, the LDS Church's doctrinal architecture would lose its most contested historical pivot. The revelatory authority structure would face a crisis of precedent (can a prophet's revelation be strategically pragmatic?). Polygamist descendants' legal and spiritual status would be unresolved. The federal-church settlement (statehood, property, Smoot) would lose its legitimating narrative. The world rearranges because the constraint's doctrinal and institutional residue is load-bearing.
% FOUNDING_PROBLEM: Federal coercion (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatened the LDS Church with disincorporation, total property seizure, leadership imprisonment, and the end of its institutional existence. Plural marriage was the specific practice targeted; the founding problem was institutional survival under superior coercive power.
% FOUNDING_PROBLEM_CORROBORATION: Federal enforcement records, Supreme Court decisions, and the church's own property restoration and statehood (1896) corroborate that the existential coercive threat was substantially resolved by 1896-1907. The Smoot hearings (1904-1907) confirmed the church's political rehabilitation. No credible attestation from outside the beneficiary set (church leadership) maintains that the 1890-level existential threat persisted past 1910. The arrangement persists despite the founding problem's resolution — classic mandatrophy.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78 at interval end) because the constraint extracts compliance, family disruption, legal risk, and cognitive dissonance from polygamists and monogamists while leadership gains survival and legitimacy. Suppression is high (0.82) because the constraint's persistence depends on active enforcement of the public/private divergence — temple recommend interviews, public denials, excommunication of dissidents. Theater ratio (0.45) reflects the real coordination function (institutional survival) entangled with performative maintenance of the revelation narrative. The constraint is tangled_rope: genuine coordination (preserving the institution) AND asymmetric extraction (leadership survives, polygamists pay).
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat: the constraint is a scaffold/tangled_rope — a temporary, painful adaptation that preserves the salvific institution. From the coerced polygamist seat: it is a snare — doctrine still binds them, but practice is criminalized, and leadership's secret authorizations betray them. From the deceived monogamist seat: it is a piton — the constraint persists as theatrical doctrine with no functional purpose for them. The engine computes these per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is the structural beneficiary (d near 0.0) — they collect institutional survival, property restoration, political rehabilitation, and narrative control. Coerced polygamists are full targets (d near 1.0) — they bear legal jeopardy, family disruption, and spiritual crisis with trapped exit. Deceived monogamists are constrained targets (d ~0.6-0.7) — they bear cognitive dissonance and legitimacy costs with limited exit. Federal authorities are not a stakeholder seat; they are the coercive boundary condition that shapes the constraint's enforcement profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal coercion threatening institutional extinction) was live in 1890. By 1904 (Second Manifesto) and certainly by 1910, the coercive pressure had substantially abated (statehood achieved, Smoot seated, property restored). The arrangement persists because the doctrinal mandate was never formally rescinded — only suspended. This is mandatrophy: the survival function is achieved, but the constraint remains because doctrinal rescission would fracture the revelatory authority structure. The constraint type shifted from scaffold (1890, with implicit sunset) to tangled_rope (1890-1904, secret continuations) to piton (post-1910, doctrinal vestige).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_necessity,
    'Does the Manifesto''s revelation narrative represent genuine prophetic experience or post-hoc legitimation of capitulation?',
    'Comparative analysis of Woodruff''s private correspondence, diary entries, and the timing of the Manifesto relative to the Edmunds-Tucker Act enforcement and Supreme Court decisions (Davis v. Beason, 1890).',
    'If revelation is genuine, the constraint is endogenous_reinterpretation (rope/scaffold). If post-hoc legitimation, it is institutional_pragmatism (tangled_rope/snare) with leadership as beneficiary of the narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_necessity, conceptual, 'The epistemic status of the revelatory claim — the core axis of the kernel''s contestation.').

omega_variable(
    secret_continuation_extraction,
    'Did post-Manifesto plural marriages (1890-1904) represent leadership-authorized continuation or unauthorized defiance?',
    'Analysis of temple records, affidavits from the Smoot hearings (1904-1907), and leadership correspondence regarding post-Manifesto marriages.',
    'If authorized, the constraint''s extraction continued covertly with leadership as ongoing beneficiary and polygamists as continuing victims. If unauthorized, the constraint''s enforcement shifted and the M-set gap is real but not instrumentally maintained by leadership.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secret_continuation_extraction, empirical, 'Whether the M-set gap''s victimization was instrumentally sustained by the beneficiary set.').

omega_variable(
    reading_identity_of_kernel,
    'Is the institutional_pragmatism reading a distinct constraint from the kernel, or a meta-reading of the same constraint?',
    'ε-invariance test: does this reading instantiate a structurally distinct constraint with its own beneficiary/victim structure and extraction profile, or does it redescribe the kernel''s observables?',
    'If distinct, it is a sibling constraint in the kernel family. If meta-reading, it belongs in commentary on the kernel itself, not as a separate story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_of_kernel, conceptual, 'Commitment to this reading as a separate ε-invariant constraint per DP-001.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.4).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.45).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.25).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.72).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.8).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.82).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__institutional_pragmatism_reading, 0.1).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, second_manifesto_1904).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, smoot_hearings_1904_1907).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three readings with distinct ε profiles: endogenous_reinterpretation (ε~0.15, rope/scaffold), exogenous_override (ε~0.85, snare), institutional_pragmatism (ε~0.78, tangled_rope). This reading's higher ε than endogenous_reinterpretation reflects the extraction of covert continuation; its lower ε than exogenous_override reflects the genuine coordination function (institutional survival) that exogenous_override treats as pure coercion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, institutional, 0.1).
constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, powerless, 0.95).
constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, moderate, 0.65).
constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
