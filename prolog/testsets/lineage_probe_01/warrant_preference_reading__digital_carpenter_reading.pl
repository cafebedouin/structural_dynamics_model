% ============================================================================
% CONSTRAINT STORY: warrant_preference_reading__digital_carpenter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_warrant_preference_reading__digital_carpenter_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: warrant_preference_reading__digital_carpenter_reading
 *   human_readable: Carpenter Reading: Warrant Requirement for Digital Location History
 *   domain: constitutional_law/fourth_amendment/digital_privacy
 *
 * SUMMARY:
 *   Carpenter v. United States (2018) represents a doctrinal break in Fourth
 *   Amendment jurisprudence: the Supreme Court held that law enforcement must
 *   obtain a warrant before compelling cellular service providers to disclose
 *   historical cell-site location information (CSLI) for a suspect,
 *   overriding the third-party doctrine that previously governed such
 *   requests. The Court reasoned that digital aggregation of location records
 *   constitutes a 'difference in kind' from individual third-party
 *   disclosures — the granular, continuous nature of location data creates a
 *   comprehensive picture of a person's movements and associations that was
 *   previously technologically infeasible for investigators to acquire. This
 *   constraint story instantiates the Carpenter reading of the
 *   warrant-preference kernel: the operative rule is that magistrate approval
 *   (warrant requirement) gates access to aggregated digital records,
 *   suppressing law enforcement's warrantless dragnet capacity but gating
 *   rather than eliminating law enforcement surveillance. The constraint is a
 *   Tangled Rope: it establishes genuine coordination between law enforcement
 *   and the magistracy (warranted surveillance is predictable and bounded),
 *   but also creates asymmetric extraction via exceptions (good-faith
 *   exception, administrative necessity, exigent circumstances) that route
 *   around the magistrate gate. The beneficiary is the location-tracked
 *   public and the magistrate gatekeeping authority; the victim is
 *   third-party doctrine maximalism (which loses its simplifying effect) and
 *   undifferentiated surveillance capacity.
 *
 * KEY AGENTS:
 *   - Location-Tracked Public: Primary victim (powerless/trapped) — cannot exit cellular networks; survives pervasive location surveillance but now has magistrate protection against warrantless dragnet access. Pre-Carpenter: no warrant gate, full suppression (0.95), extractiveness 1.0. Post-Carpenter: warrant gate imposes costs on surveillance but exceptions persist (suppression 0.48, extractiveness 0.58).
 *   - Magistrate Gatekeeping Authority: Primary beneficiary (organized/mobile) — gains institutional authority over surveillance scope and duration. Coordinates warrant issuance with law enforcement, establishing predictable checkpoints. Rope classification reflects pure coordination benefit.
 *   - Third-Party Doctrine Maximalists: Victim of constraint (institutional/constrained) — courts, carriers, regulatory bodies that benefited from the simplicity of third-party doctrine (no consent, no warrant). Constraint forces a new doctrinal complexity and requires carriers to implement warrant-processing systems.
 *   - Law Enforcement Surveillance Capacity: Secondary victim (powerful/arbitrage) — bears extraction costs from warrant requirement; has exit options via exceptions (good-faith, necessity, emergency). Snare perspective reflects that exceptions are real but unreliable.
 *   - Digital Civil Liberties Organizations: Beneficiary and coordinator (organized/constrained) — benefit from magistrate gatekeeping mechanism and use it to challenge dragnet surveillance; constrained by litigation costs and jurisdictional fragmentation.
 *   - Carpenter Enforcement Coalition: Mixed actor (organized/constrained) — enforces Carpenter standard through litigation and policy, but faces erosion via good-faith exception expansion. Scaffold perspective reflects generational sunset risk.
 *   - Analytical Observer: Naturalizes constitutional principle (analytical/analytical) — risks reading Carpenter as application of immutable Fourth Amendment principle rather than contingent doctrinal choice. Mountain classification is false summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(warrant_preference_reading__digital_carpenter_reading, 0.58).
domain_priors:suppression_score(warrant_preference_reading__digital_carpenter_reading, 0.48).
domain_priors:theater_ratio(warrant_preference_reading__digital_carpenter_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(warrant_preference_reading__digital_carpenter_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(warrant_preference_reading__digital_carpenter_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(warrant_preference_reading__digital_carpenter_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(warrant_preference_reading__digital_carpenter_reading, tangled_rope).
narrative_ontology:human_readable(warrant_preference_reading__digital_carpenter_reading, "Carpenter Reading: Warrant Requirement for Digital Location History").
narrative_ontology:topic_domain(warrant_preference_reading__digital_carpenter_reading, "constitutional_law/fourth_amendment/digital_privacy").

domain_priors:requires_active_enforcement(warrant_preference_reading__digital_carpenter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(warrant_preference_reading__digital_carpenter_reading, 'c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8').
narrative_ontology:cs_kernel_codification('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', fixed_text).
narrative_ontology:cs_authority_grounding('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', lineage).
narrative_ontology:cs_interpretation_layer_present('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8').
narrative_ontology:cs_reading_relation('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', warrant_preference_reading__exclusionary_rule_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', warrant_preference_reading__good_faith_exception_reading, coexists_with).
narrative_ontology:cs_axiom('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', foundational, digital_aggregation_structural_difference).
narrative_ontology:cs_axiom_status(digital_aggregation_structural_difference, holdable).
narrative_ontology:cs_axiom_grounding('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', digital_aggregation_structural_difference, empirically_contingent).
narrative_ontology:cs_axiom('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', foundational, magistrate_gatekeeping_as_structural_suppression).
narrative_ontology:cs_axiom_status(magistrate_gatekeeping_as_structural_suppression, holdable).
narrative_ontology:cs_axiom_grounding('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', magistrate_gatekeeping_as_structural_suppression, deontological).
narrative_ontology:cs_reference_frame('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', warrant_requirement_digital_aggregation).
narrative_ontology:cs_drift_state('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', post_carpenter_exception_expansion, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c5e7a2b0-ae0a-4d4d-87b3-a05f3e847db8', '2026-02-27T14:32:18Z').
narrative_ontology:cs_kernel_id(warrant_preference_reading__digital_carpenter_reading, warrant_preference_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(warrant_preference_reading__digital_carpenter_reading, location_tracked_public).
narrative_ontology:constraint_beneficiary(warrant_preference_reading__digital_carpenter_reading, magistrate_gatekeeping_authority).
narrative_ontology:constraint_victim(warrant_preference_reading__digital_carpenter_reading, third_party_doctrine_maximalism).
narrative_ontology:constraint_victim(warrant_preference_reading__digital_carpenter_reading, dragnet_surveillance_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCATION-TRACKED PUBLIC (SNARE) — Cannot exit cellular networks or digital infrastructure. Bears full extraction via pervasive location surveillance. Before Carpenter, suppression was nearly total: third-party doctrine meant no warrant gate, no privacy claim, no remedy. Carpenter gated extraction but did not eliminate it — the magistrate requirement is enforcement, not elimination. Still snare-classified because the gate remains subject to exception, necessity, and administrative routine.
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DIGITAL CIVIL LIBERTIES ORGS (TANGLED ROPE) — Constrained by resource limits and litigation costs; benefit from coordination function of Carpenter doctrine (establishes magistrate gatekeeping standard) but bear costs of coordinating across fragmented jurisdictions and post-Carpenter erosion via good-faith exception and administrative necessity claims. Mixed experience: the doctrine provides a mechanism for challenging dragnet surveillance (coordination function) but the mechanism is partially disabled (asymmetric extraction via exceptions).
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAGISTRATE GATEKEEPING AUTHORITY (ROPE) — Organized institutional actor (judiciary) with mobility via appellate and legislative processes. Carpenter doctrine coordinates judicial review of digital surveillance: law enforcement must seek warrants from magistrates, establishing predictable checkpoints. Low net extraction for magistrates — they gain institutional authority over surveillance questions. Pure coordination: gating structure enables predictable scope and duration limits on warrants, reducing arbitrary surveillance and protecting magistrate legitimacy.
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT SURVEILLANCE CAPACITY (SNARE) — Powerful actor with arbitrage options (administrative necessity, exigent circumstance, good-faith exception exceptions to warrant requirement). Experiences Carpenter as an extraction constraint on their surveillance authority. Net victim of the constraint: must seek warrants (cost and delay) or route around the magistrate gate (administrative exception). Classified as snare because the extraction (warrant requirement) is genuine but enforcement depends on exception-handling and prosecutorial discretion in seeking warrants.
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THIRD-PARTY DOCTRINE MAXIMALISTS (TANGLED ROPE) — Institutional actors (courts, regulatory bodies, communications carriers) who benefit from third-party doctrine as a simplifying framework (no consent requirement, no warrant, carriers cooperate routinely without judicial process). Carpenter constrains this benefit but does not eliminate it — the doctrine survives for prospective data, narrowly defined. Tangled: the constraint creates mixed extraction (warrant requirement for historical bulk records) and coordination (established boundaries between third-party and first-party doctrine). Victim of the constraint in that their simplifying regime is partially dismantled.
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CARPENTER ENFORCEMENT COALITION (SCAFFOLD) — Organized actors (EFF, civil rights organizations, privacy advocates, sympathetic judges) enforcing the Carpenter standard through litigation and policy advocacy. Scaffold classification derives from generational time horizon and constrained exit: the doctrine is actively enforced and expanding (warrant requirements spreading to other digital records — SMS metadata, location pings from emergency calls), but enforcement is incomplete and decaying via exceptions. Theater ≤ 0.35: warrant procedures are substantive, not performative; magistrates exercise real discretion. The constraint has a sunset risk: if law enforcement develops technical workarounds (administrative exception expansion, necessity claims) faster than doctrine expansion, the magistrate gate collapses.
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Fourth Amendment's warrant requirement represents an immutable constitutional principle: no government seizure of papers or effects without judicial oversight. Carpenter doctrine is read as simply applying this fixed principle to digital data. The constraint appears immutable because it derives from constitutional text and ratifying consensus. However, this perspective risks naturalizing what is actually a jurisdictional choice made by the Supreme Court in Carpenter — the good-faith exception reading demonstrates that the same constitutional text can ground different operative constraints. False summit candidate: the 'constitutional natural law' framing naturalizes a 2018 interpretive choice.
constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(warrant_preference_reading__digital_carpenter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(warrant_preference_reading__digital_carpenter_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(warrant_preference_reading__digital_carpenter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(warrant_preference_reading__digital_carpenter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate. Pre-Carpenter (t=0), law enforcement could access location history without warrant; extractiveness was 1.0 (total surveillance capacity, no magistrate gate). Post-Carpenter (t=6), warrant requirement suppresses dragnet access but exceptions (good-faith, necessity) enable routing around the gate. The extractiveness value reflects the asymmetry: magistrate approval is required, but approval is largely routine (warrants granted at ~95% rate post-Carpenter in practice), and exceptions expand (exigent circumstance, administrative necessity, emergency calls). By t=8, good-faith exception and administrative necessity exceptions have expanded, pushing extractiveness toward 0.63. The value of 0.58 represents a stable intermediate state where the warrant gate operates but is substantially disabled by exceptions and prosecutorial discretion. Suppression (0.48): Moderate. Pre-Carpenter, suppression was near-total (0.95) — third-party doctrine meant no meaningful barrier to access, and the public had no effective remedy against warrantless surveillance. Post-Carpenter, suppression drops because the warrant requirement is an enforceable barrier: if law enforcement obtains CSLI without a warrant (or with a warrant but outside Carpenter scope), the evidence can be suppressed (exclusionary rule). Suppression is not elimination (0.48 not 0.0) because exceptions exist and suppression doctrine itself has limits. Theater ratio (0.35): Low-moderate. Carpenter warrants are substantive, not performative — magistrates actually review law enforcement's justification for surveillance and can deny warrant requests that lack probable cause or statutory particularity. The theater is not zero because warrant procedures are routine and some magistrates grant warrants formulaically. Theater ratio is lower than many procedural mechanisms because the Fourth Amendment stakes are high and appellate review is available. The increase from 0.05 pre-Carpenter (third-party doctrine involved no magistrate participation, hence minimal theater) to 0.35 post-Carpenter reflects the addition of a warrant procedure.
 *
 * PERSPECTIVAL GAP:
 *   The Carpenter reading instantiates multiple incompatible perspectives on the same structural data. Law enforcement perspectives emphasize the extraction costs of the warrant requirement (snare, powerful/biographical). Magistrate perspectives emphasize the coordination benefit of gating surveillance authority (rope, organized/generational). Civil liberties perspectives emphasize the tangled extraction-coordination mix (tangled rope, moderate/biographical). The third-party doctrine maximalist perspectives emphasize the constraint as victim status — their simplifying regime is dismantled (tangled rope, institutional/constrained). The location-tracked public perspectives shift from snare pre-Carpenter (no gate, total suppression) to snare post-Carpenter (gate exists but exceptions and routine grants persist). The analytical/civilizational perspective risks naturalizing the Carpenter choice as immutable constitutional law, missing the fact that the good-faith exception reading would authorize a different operative constraint (warrant requirement with broad exceptions → snare for the public). The perspectival gap reveals that the kernel (warrant preference in digital surveillance) is contested: Carpenter resolves it one way (warrant requirement gates dragnet access), but the good-faith exception reading would resolve it differently (warrant requirement exists but exceptions often subsume the gate), and the exclusionary-rule reading would emphasize remedy (suppression) over gating.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position of each agent relative to the constraint. The location-tracked public is trapped with no exit from cellular networks; law enforcement is powerful with arbitrage options (exceptions to warrant requirement). The magistrate is an organized institutional actor with mobility via appellate and legislative processes; their power derives from the coordinating function the constraint assigns them. Beneficiaries (location-tracked public, magistrate authority) derive low or negative d values from the engine's derivation chain: the public benefits from suppression of warrantless surveillance; magistrates benefit from institutional authority over surveillance scope. Law enforcement, as a victim of the warrant requirement, derives high d (powerful actor experiencing extraction costs from the gate). Third-party doctrine maximalists, as institutional victims, derive moderate d. The perspectival gap is large: law enforcement sees snare (extraction via warrant requirement, exceptions available but unpredictable), while magistrates see rope (pure coordination). The location-tracked public sees snare pre-Carpenter (no gate) and tangled rope post-Carpenter (mixed benefit from warranty requirement and cost from exceptions and routine grant rates).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CONSTRAINT: The mandatrophy on this constraint is resolved by recognizing that it is ONE READING of a contested kernel, not an attempt to classify the Fourth Amendment warrant requirement simpliciter. The mandatrophy (classification ambiguity when extractiveness > 0.70) does not apply here because the Carpenter reading keeps extractiveness at 0.58, below the mandatrophy threshold. However, the good-faith_exception_reading would push extractiveness to 0.72+ (warrant gate substantially disabled by exceptions), triggering the mandatrophy gate for that reading. This illustrates the critical principle: the same constitutional text and historical facts support multiple readings with different extractiveness values and different classifications. The Carpenter reading is tangled_rope (warrant gate + exceptions); the good-faith reading is snare (warrant requirement largely nominal, exceptions dominant); the exclusionary_rule reading is rope (warrant rules + suppression remedy for violations). Each reading is internally consistent and empirically grounded. The perspectival gap is not failure of classification but success of the framework in capturing the genuine structural ambiguity in how the Fourth Amendment constrains digital surveillance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bulk_vs_targeted_warrant_requirement_boundary,
    'Does the Carpenter warrant requirement apply to prospective ongoing location tracking authorized by a single warrant, or only to retrospective bulk historical records?',
    'Appellate litigation clarifying the temporal and quantitative scope of Carpenter warrants. Jurisdictional development of warrant standards for continuous monitoring vs episodic access.',
    'If boundary narrows (prospective tracking exempt): extractiveness drops to 0.35, doctrine shifts toward rope. If boundary expands (warrant required for all location gating): extractiveness rises to 0.68, doctrine shifts toward tangled_rope with higher victim load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bulk_vs_targeted_warrant_requirement_boundary, empirical, 'Boundary between bulk historical records and prospective tracking in warrant requirement scope').

omega_variable(
    good_faith_exception_erosion_rate,
    'How quickly will law enforcement exploit the good-faith exception to obtain location records without Carpenter-compliant warrants, and will judicial review catch the drift?',
    'Longitudinal empirical study of CFAA warrants granted vs Carpenter-style warrants granted post-2018. Tracking of appellate reversal rates for good-faith exception claims involving location data. Measurement of actual law enforcement compliance with Carpenter in field operations.',
    'If erosion is rapid (>50% of location requests route via good-faith exception): doctrine collapses toward snare (extraction via exception). If erosion is slow (<10%): doctrine stabilizes as tangled_rope with enforcement credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(good_faith_exception_erosion_rate, empirical, 'Rate at which good-faith exception undermines Carpenter warrant requirement in practice').

omega_variable(
    digital_aggregation_as_difference_in_kind_dispute,
    'Is digital aggregation of location records a ''difference in kind'' from individual third-party disclosures (Carpenter''s core reasoning), or a quantitative scaling of an existing principle?',
    'Jurisprudential development: Supreme Court clarification of whether the ''difference in kind'' concept extends to other digital aggregations (email metadata, financial records, communication patterns). Circuit split resolution on whether Carpenter reasoning is generalizable or sui generis to location data.',
    'If ''difference in kind'' is deemed sui generis: Carpenter does not expand beyond CSLI; other digital dragnet constraints are ruled mountain (natural consequence of third-party doctrine). If ''difference in kind'' is generalizable: similar warrant gates spread to email metadata, call records, financial aggregation; multiple tangled_rope constraints emerge with expanded victim sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_aggregation_as_difference_in_kind_dispute, conceptual, 'Whether Carpenter''s ''difference in kind'' reasoning is generalizable to other digital aggregations').

omega_variable(
    reading_contest_kernel_indexing,
    'Which reading of the warrant-preference kernel does Carpenter instantiate: the digital_carpenter_reading (aggregation as difference in kind), the exclusionary_rule_reading (remedy through suppression), or the good_faith_exception_reading (deterrence-focused exceptions)?',
    'The kernel contest is not empirically resolvable — different readings coexist as live jurisprudential positions held by different judges and institutions. However, the DIRECTION of jurisprudential development will reveal which reading''s authority is ascending: if Supreme Court expands Carpenter (more data types covered, exceptions narrowed), digital_carpenter_reading ascends; if exceptions expand faster than doctrine, good_faith_exception_reading ascends; if exclusionary rule is narrowed or superseded by damage remedies, exclusionary_rule_reading declines.',
    'The constraint''s extractiveness value (0.58) is stable only if digital_carpenter_reading remains dominant. If good_faith_exception_reading ascends, extractiveness rises toward 0.70+ (snare). If exclusionary_rule_reading strengthens (broader suppression scope, narrower exceptions), extractiveness drops toward 0.40 (rope). The three readings are not alternative measurements of the same constraint — they are distinct constraints with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_indexing, conceptual, 'Which reading of the warrant-preference kernel is the operative constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(warrant_preference_reading__digital_carpenter_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warrant_carpenter_theater_pre_carpenter, warrant_preference_reading__digital_carpenter_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(warrant_carpenter_theater_post_carpenter_2018, warrant_preference_reading__digital_carpenter_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(warrant_carpenter_theater_exception_expansion, warrant_preference_reading__digital_carpenter_reading, theater_ratio, 8, 0.38).

% Extraction over time
narrative_ontology:measurement(warrant_carpenter_extractiveness_pre_carpenter, warrant_preference_reading__digital_carpenter_reading, base_extractiveness, 0, 1.0).
narrative_ontology:measurement(warrant_carpenter_extractiveness_post_carpenter_2018, warrant_preference_reading__digital_carpenter_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(warrant_carpenter_extractiveness_exception_expansion, warrant_preference_reading__digital_carpenter_reading, base_extractiveness, 8, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(warrant_carpenter_suppression_pre_carpenter, warrant_preference_reading__digital_carpenter_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(warrant_carpenter_suppression_post_carpenter_2018, warrant_preference_reading__digital_carpenter_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(warrant_carpenter_suppression_exception_expansion, warrant_preference_reading__digital_carpenter_reading, suppression_requirement, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(warrant_preference_reading__digital_carpenter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(warrant_preference_reading__digital_carpenter_reading, third_party_doctrine_dragnet_access).
narrative_ontology:affects_constraint(warrant_preference_reading__digital_carpenter_reading, good_faith_exception_warrant_exception).
narrative_ontology:affects_constraint(warrant_preference_reading__digital_carpenter_reading, exclusionary_rule_remedy_scope).

% DUAL FORMULATION NOTE:
% The Carpenter reading (this constraint) assumes warrant requirement gates CSLI access. The good-faith exception reading assumes exceptions substantially disable the warrant gate. The exclusionary rule reading assumes suppression is the primary enforcement mechanism. Each reading instantiates a different constraint with different ε values (0.58 vs 0.72+ vs 0.42). They are linked via network.affects_constraints because case law and policy development upstream (magistrate gatekeeping, exception expansion, remedial scope) affects downstream reading dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
