% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Article VI Reading: Non-Proliferation (Binding) vs. Disarmament (Aspirational)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (entered into force 1970) is a
 *   contested kernel grounding international nuclear governance. The written
 *   text (Article VI) contains an ambiguous commitment: NWS pledge to pursue
 *   'cessation of the nuclear arms race at an early date' and 'nuclear
 *   disarmament.' This reading instantiates how NWS have interpreted that
 *   obligation: as an aspirational goal without binding timeline, enabling
 *   indefinite deferral of disarmament while NNWS bear strict
 *   non-proliferation verification burdens. The constraint is a tangled_rope:
 *   it generates genuine coordination benefits (prevents horizontal
 *   proliferation, enables nuclear fuel trade, provides security framework)
 *   alongside extractive asymmetry (NWS preserve strategic arsenals; NNWS
 *   accept permanent verification; disarmament obligation remains perpetually
 *   unmet and unenforced). The theater_ratio has risen from 0.42 (1970, when
 *   disarmament seemed plausible within a generation) to 0.58 (2015, when NWS
 *   arsenal modernization and NNWS disappointment over repeated failed
 *   disarmament timelines made the performative gap visible). Extractiveness
 *   has risen as NNWS have organized around disarmament demands, only to see
 *   NWS use interpretive authority to defer obligations. This story
 *   represents one reading of the NPT kernel; the sibling nnws_reading
 *   instantiates the opposite interpretation (disarmament as binding), and
 *   the withdrawal_threshold_reading examines the point at which NNWS exit
 *   options become rational.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (NWS: US, Russia, China, France, UK): Institutional/arbitrage — benefit from treaty's coordination function while preserving strategic freedom through interpretive control of Article VI; define disarmament as indefinitely deferrable
 *   - Non-Nuclear Weapon States (NNWS Coalition: NAM, G77, Sweden, Austria, Brazil): Organized/constrained — bear verification burdens and technology restrictions; constrained by security guarantees and fuel supply dependencies; increasingly extract high cost from indefinite deferral
 *   - IAEA Safeguards Division: Institutional/arbitrage — maintains verification bureaucracy; budget concentration on horizontal proliferation reflects NWS preference; piton perspective shows institutional inertia
 *   - Non-Aligned Movement (NAM): Organized/constrained — collective voice for NNWS disarmament demands; constrained by inability to enforce Article VI against NWS resistance; annual Review Conference advocacy largely performative
 *   - Middle Powers / Disarmament Advocates (Ireland, Austria, Aotearoa New Zealand, ICAN): Organized/mobile — pushing for treaty amendment and binding disarmament timelines; mobile because can pursue regional treaties and extra-NPT coalitions
 *   - Global Disarmament Verification Infrastructure (conceptual victim): Powerless/trapped — theoretical capacity to verify disarmament remains perpetually underfunded and undeployed because NWS resist comparable inspection regimes on their own arsenals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.54).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Article VI Reading: Non-Proliferation (Binding) vs. Disarmament (Aspirational)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37').
narrative_ontology:cs_kernel_codification('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', fixed_text).
narrative_ontology:cs_authority_grounding('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', extraction).
narrative_ontology:cs_interpretation_layer_present('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37').
narrative_ontology:cs_reading_relation('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', nnws_reading__article_vi_obligation, forecloses).
narrative_ontology:cs_reading_relation('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', foundational, article_vi_aspirational_disarmament).
narrative_ontology:cs_axiom_status(article_vi_aspirational_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', article_vi_aspirational_disarmament, conventional).
narrative_ontology:cs_axiom('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', foundational, nws_interpretive_authority_over_treaty).
narrative_ontology:cs_axiom_status(nws_interpretive_authority_over_treaty, holdable).
narrative_ontology:cs_axiom_grounding('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', nws_interpretive_authority_over_treaty, conventional).
narrative_ontology:cs_reference_frame('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', reciprocal_disarmament_obligation).
narrative_ontology:cs_drift_state('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', contemporary_arsenal_modernization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8549d67-fd67-4b27-b2f7-ee7d3b3f0e37', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nws_security_establishment).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, nnws_treaty_compliance_burden).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, global_disarmament_verification_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NNWS UNDER NWS READING (SNARE) — Trapped by NPT Article III safeguards obligations (IAEA inspections, fuel cycle monitoring, material accounting) with no enforcement mechanism for NWS disarmament. NNWS bear extraction: verification costs, restrictions on fuel production, sensitive technology denial, while the disarmament obligation on NWS remains unenforceable and perpetually deferred. Zero exit options — withdrawal triggers non-proliferation sanctions; compliance continues indefinitely without reciprocal NWS constraints on arsenals.
constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED MOVEMENT (TANGLED ROPE) — Organized NNWS (India, Brazil, South Africa, NAM members) benefit from the treaty's coordination function: it reduces horizontal proliferation risks, enables fuel supply agreements, and provides a unified negotiating bloc for disarmament advocacy. But they also bear asymmetric extraction: the 'at an early date' language in Article VI is interpreted by NWS as indefinitely deferrable, while NNWS verification obligations grow stricter with each Review Conference. Coalition has partial agency (can demand compliance timelines, can threaten withdrawal) but constrained by dependence on NWS-controlled fuel and security guarantees. Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR WEAPON STATES (ROPE) — Under the NWS reading, NWS experience the NPT as pure coordination: the treaty stabilizes the international system by locking NNWS into non-proliferation obligations while preserving NWS strategic freedom. The disarmament clause (Article VI) is interpreted as aspirational framing without temporal constraint ('at an early date' is indefinite). NWS benefit from institutional control of treaty interpretation (through the Review Conference chair positions, IAEA Board seats, and P5 Security Council authority). Effective extractiveness is low from the NWS perspective — they see coordination benefits (stability, legitimacy, NNWS compliance) without corresponding obligations.
constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL DISARMAMENT ADVOCATES (SCAFFOLD) — Middle powers and civil society (Ireland, Austria, Aotearoa New Zealand, ICAN) see the NWS reading as a temporary institutional arrangement with an implicit sunset. They advocate for treaty amendment (Article VIII review mechanism, Article X withdrawal process revision) that would make disarmament binding or create trigger points for treaty renegotiation. This perspective treats the current interpretation as scaffolding for future, more stringent regimes. Mobile exit options (can pursue regional treaties like African Nuclear Weapon Free Zone, can build coalitions outside formal NPT structure). Theater ratio relatively low — their advocacy is directly functional (attempting to shift interpretation), not performative.
constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: IAEA SAFEGUARDS BUREAUCRACY (PITON) — The IAEA safeguards system persists as a degraded proxy for disarmament verification. Originally conceived as a temporary measure pending NWS disarmament, IAEA verification has become institutionalized as an end-in-itself: budget allocations concentrate on NNWS horizontal proliferation detection (70% of inspection resources) while NWS vertical arsenal accounting receives minimal scrutiny (5-10% of capacity). The IAEA's institutional interest in perpetuating its mandate mirrors NWS interest in maintaining indefinite strategic freedom. Theater ratio 0.58 reflects that much IAEA activity is procedurally authentic (inspections are real) but functionally degraded (cannot verify NWS disarmament claims with current mandate). Piton classification: maintains NWS reading through institutional inertia, not genuine verification function.
constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational perspective, the tension between non-proliferation and disarmament could appear as an immutable feature of nuclear deterrence: any system that prevents horizontal proliferation must preserve vertical strategic asymmetry (NWS arsenals deterring both each other and adversaries). This naturalization treats the NWS reading's indefinite deferral of disarmament as a law of international stability — the price of preventing proliferation. However, this perspective is diagnostically suspect — the structural data reveals NWS beneficiaries and NNWS victims, indicating the mountain is a false summit: the 'natural law' framing naturalizes what is a contingent institutional arrangement that benefits those who control its interpretation.
constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_treaty_text__nws_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_treaty_text__nws_reading, TR),
    TR >= 0.70.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.54: Moderate-high. The NWS reading extracts asymmetric benefit through interpretive control. NWS gain from NNWS non-proliferation compliance (reduces competitors, stabilizes strategic environment) while avoiding reciprocal disarmament constraints. The extraction is not maximal (snare) because genuine coordination benefits exist — the treaty does reduce horizontal proliferation and provides a framework for fuel supply. But the extraction is significant because the disarmament obligation is indefinitely deferred without penalty, and NNWS verification burdens grow with each Review Conference. Extraction has risen from 0.35 (1970, when disarmament seemed imminent) to 0.54 (2015, as the indefinite deferral became structurally embedded and NWS made explicit that 'at an early date' meant decades or longer). Suppression 0.62: High. NNWS face substantial barriers to exit (Article X withdrawal triggers sanctions, security guarantees are revoked, access to fuel and technology is denied) and barriers to enforcing Article VI (no verification regime, no enforcement mechanism, NWS veto power in Security Council). Theater ratio 0.58: Moderate. The constraint involves authentic verification activity (IAEA inspections are real) but significant performative content (Review Conference disarmament declarations are repeatedly broken, NWS arsenal modernization contradicts stated disarmament intentions, IAEA budget allocation deprioritizes NWS verification). The rise from 0.42 to 0.58 reflects growing divergence between stated disarmament rhetoric and actual NWS arsenal expansion.
 *
 * PERSPECTIVAL GAP:
 *   The classification gap is maximal across the six perspectives, making this a diagnostic exemplar for perspectival inversion. NWS see rope (pure coordination benefits); NNWS trapped agents see snare (pure extraction, no exit); NNWS organized agents see tangled_rope (mixed coordination-extraction); disarmament advocates see scaffold (temporary arrangement with sunset); IAEA sees piton (degraded institutional function); analytical observer risks mountain (naturalizing as immutable feature of nuclear deterrence). The gap reveals that the NWS reading is actively produced through interpretive authority: NWS control treaty interpretation through Review Conference chair positions, IAEA Board representation, and P5 Security Council veto. The nnws_reading (opposite interpretation) remains live as NNWS demand enforcement but cannot impose their reading. The perspectival gap IS the constraint — it instantiates the asymmetry of interpretive power.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) reflects their structural position relative to the extraction flow under the NWS reading. NWS institutional beneficiaries with arbitrage exit options derive d ≈ 0.05-0.15 (full beneficiary status), producing low/negative effective extractiveness chi through the sigmoid f(d). NNWS trapped agents with no exit option derive d ≈ 0.95 (full target status), producing maximum chi ≈ 1.35+. NNWS organized agents with constrained exit derive d ≈ 0.70-0.80 (moderate targets), producing chi ≈ 0.80-1.00. The IAEA institutional actor with institutional benefit derives d ≈ 0.25 (partial beneficiary — the IAEA's mandate is sustained by the NWS reading), but piton classification derives from theater_ratio gate rather than chi. The analytical observer with analytical perspective derives d ≈ 0.72-0.75 (observer baseline), producing chi ≈ 1.15 characteristic of analytical perspective. No directionality overrides are needed — the structural derivation chain produces accurate values.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint exhibits different types from different structural positions and time horizons. At the NWS institutional/immediate perspective, it is rope (pure coordination). At the NNWS powerless/trapped/generational perspective, it is snare (pure extraction). At the NNWS organized/constrained/generational perspective, it is tangled_rope (mixed). The constraint is not a single type — it is a presheaf over the observation site. The NWS reading instantiates this multi-typed structure by making the disarmament obligation perpetually aspirational rather than binding: this allows NWS to experience the treaty as beneficial coordination while NNWS experience it as extractive asymmetry. The false summit mountain perspective reveals the naturalization mechanism: treating the NWS interpretation as an immutable law of nuclear deterrence naturalizes what is a contingent institutional choice (NWS control over interpretive authority). The mandatrophy is diagnostically productive — it shows that the constraint's classification depends entirely on which reading of Article VI is treated as authoritative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_temporal_ambiguity,
    'Does Article VI''s ''cessation of the nuclear arms race at an early date'' impose a legally binding disarmament obligation with an implicit or explicit deadline, or is it a non-binding aspirational goal?',
    'International Court of Justice advisory opinion on treaty interpretation (requested by WHO 1996, delivered with ambiguous language on temporal constraint); subsequent treaty amendment process clarifying ''at an early date'' with specific timelines; state practice patterns (NWS compliance with disarmament commitments vs. continued arsenal modernization)',
    'If binding with deadline: NWS reading collapses into nnws_reading (mutual obligations); extraction reverses. If binding without deadline: current tangled_rope classification holds but with higher perceived extraction for NNWS (permanent obligation without reciprocal constraint). If non-binding: NWS reading confirmed; NNWS extraction increases; nnws_reading becomes incoherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_temporal_ambiguity, conceptual, 'Whether Article VI imposes binding disarmament obligation or aspirational goal').

omega_variable(
    safeguards_budget_allocation_mechanism,
    'Is the IAEA''s 70/30 budget split (horizontal/vertical proliferation) a structural necessity of verification capacity or a contingent political choice reflecting NWS preference for NNWS-focused oversight?',
    'IAEA budget allocation history (1968-present) correlated with P5 pressure patterns; cost-benefit analysis of NWS verification scenarios (what would it cost IAEA to conduct real-time warhead accounting on NWS arsenals?); counterfactual: if P5 committed resources, could IAEA verify NWS disarmament claims?',
    'If structural necessity: IAEA piton classification is forced by technical constraints. If political choice: budget allocation is a signature of NWS reading''s extractive asymmetry — evidence that disarmament verification is actively deprioritized to maintain NWS strategic freedom. Changes how piton vs. tangled_rope classification should weight the institutional actor (IAEA).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguards_budget_allocation_mechanism, empirical, 'Whether IAEA budget allocation reflects technical necessity or political choice').

omega_variable(
    nnws_exit_threshold_mechanism,
    'At what NNWS extraction threshold (verification burden, technology denial, security vulnerability) does the rational exit option (Article X withdrawal) become preferable to continued compliance?',
    'Game-theoretic modeling of NNWS compliance calculations; empirical cases of actual withdrawal (N. Korea 2003) or near-withdrawal (Iran 2015-2019); threshold testing via simulation of strengthened verification or tightened sanctions',
    'If threshold is low (easily breached): NNWS have real exit option; extraction is constrained; classification should shift toward ''constrained'' rather than ''trapped'' exit_options. If threshold is high (rare): NNWS are effectively trapped; extraction maximum; snare classification confirmed across more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_exit_threshold_mechanism, empirical, 'NNWS rational exit threshold in extraction vs. benefit calculation').

omega_variable(
    nws_reading_vs_nnws_reading_foreclosure,
    'Does the NWS reading (disarmament as indefinitely deferrable aspiration) logically foreclose the NNWS reading (disarmament as binding obligation with enforcement), or do both remain live positions in competing frameworks?',
    'Textual analysis of Article VI language (grammar, tense, conditional vs. mandatory constructions); Vienna Convention on Law of Treaties principles applied to both readings; analysis of treaty negotiating history and subsequent state practice patterns',
    'If forecloses: one reading rules out the other; only one can be correct under unified interpretation framework. If coexists_with: both are live positions; the NPT is a contested kernel with no single authoritative reading. Engine classification changes based on which reading''s axioms are held to be true by the observer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nws_reading_vs_nnws_reading_foreclosure, conceptual, 'Logical relationship between NWS and NNWS readings of Article VI').

omega_variable(
    iaea_verification_sufficiency_assumption,
    'Can the IAEA''s current safeguards system, if extended to NWS vertical arsenals, actually verify disarmament claims with sufficient confidence to enable treaty enforcement, or is verification fundamentally limited by technical barriers (warhead inaccessibility, counting ambiguity, secret test facilities)?',
    'Technical assessment of NWS warhead verification protocols (intrusive inspection capacity, tamper-proofing methods, seal reliability); comparison with NNWS material accounting verification performance; analysis of why NWS have refused to allow comparable verification regimes',
    'If sufficiency possible: disarmament verification is a technical and political problem (NWS refusal); NNWS reading becomes structurally coherent. If sufficiency impossible: indefinite deferral is a technical necessity, not an extraction mechanism; NWS reading gains support; nnws_reading becomes incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_verification_sufficiency_assumption, empirical, 'Whether IAEA verification can sufficiently verify NWS disarmament').

omega_variable(
    commitment_system_kernel_stability,
    'Is the NPT kernel (the written treaty text) stable, or is its normative content being actively rewritten through interpretive practice?',
    'Textual drift analysis: comparing stated NWS positions across Review Conference declarations (1975-2022); analysis of IAEA interpretive statements diverging from treaty language; empirical observation of whether subsequent practice is constraining NWS behavior or enabling NWS to redefine treaty obligations',
    'If kernel stable: current interpretation is fixed and contestable. If kernel unstable (interpretively rewritten): NWS reading is actively suppressing alternative readings through authority over adjudication. Changes omega variable on foreclosure: if NWS reading is winning interpretively, it may functionally foreclose NNWS reading even without logical foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_system_kernel_stability, empirical, 'Whether NPT kernel is stable or interpretively drifting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nws_theater_1970, npt_treaty_text__nws_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(npt_nws_theater_1985, npt_treaty_text__nws_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(npt_nws_theater_2000, npt_treaty_text__nws_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement(npt_nws_theater_2015, npt_treaty_text__nws_reading, theater_ratio, 45, 0.58).

% Extraction over time
narrative_ontology:measurement(npt_nws_extract_1970, npt_treaty_text__nws_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(npt_nws_extract_1985, npt_treaty_text__nws_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(npt_nws_extract_2000, npt_treaty_text__nws_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(npt_nws_extract_2015, npt_treaty_text__nws_reading, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_nws_suppression_1970, npt_treaty_text__nws_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(npt_nws_suppression_1985, npt_treaty_text__nws_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(npt_nws_suppression_2000, npt_treaty_text__nws_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(npt_nws_suppression_2015, npt_treaty_text__nws_reading, suppression_requirement, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nnws_reading__article_vi_obligation).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_verification_budget_horizontal_bias).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nws_arsenal_modernization_program).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, regional_nuclear_free_zones).

% DUAL FORMULATION NOTE:
% The NPT is a contested kernel. This story (npt_treaty_text__nws_reading) models the NWS interpretation. The sibling nnws_reading__article_vi_obligation models the NNWS interpretation. Both read from the same written text but reach opposite conclusions about whether disarmament is binding. The network links show the constraint family: IAEA verification budget allocation is an enforcement-layer constraint that implements the NWS reading; NWS arsenal modernization is an empirical observation that violates the NNWS reading; regional nuclear-free zones are downstream constraints that actors use to circumvent the NPT's asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
