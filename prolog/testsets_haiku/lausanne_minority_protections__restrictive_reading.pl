% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections: Restrictive Reading (Individual Worship Only)
 *   domain: international_law/religious_governance
 *
 * SUMMARY:
 *   The restrictive reading of Lausanne minority protections (Articles 39-44)
 *   interprets the treaty as guaranteeing individual worship rights only,
 *   placing institutional autonomy, property ownership, theological
 *   education, and legal personhood of minority religious institutions under
 *   general Turkish domestic law. This reading constrains minority
 *   institutions to individual-worship framing while denying them the
 *   institutional capacity to operate as autonomous entities. The constraint
 *   operates through legal reinterpretation: the same treaty text that
 *   sibling readings interpret as protecting minority institutional
 *   continuity is here read as deferring that protection to domestic law. The
 *   extraction rises over time as the state apparatus increasingly exercises
 *   the legal authority the restrictive reading grants it — closing
 *   theological schools (1971), denying property claims, refusing legal
 *   personhood for minority religious bodies. By 2024, the constraint has
 *   evolved from a legal doctrine into systematic institutional suppression.
 *
 * KEY AGENTS:
 *   - Turkish state apparatus: agenda-setter and primary beneficiary; consolidates unilateral control over minority institutional capacity
 *   - Greek Orthodox minority: powerless victim; identity-locked (religious identity makes exit unthinkable); faces institutional foreclosure, property confiscation, clergy-training denial
 *   - Armenian Apostolic minority: powerless victim; identity-locked; denied legal personhood and educational autonomy
 *   - Jewish minority: moderate power victim; constrained (relative to other minorities) by legal limits on institutional autonomy and property rights
 *   - European human rights bodies & guarantor states: structurally excluded by the reading itself (which asserts Turkish domestic law is the frame, foreclosing international supervision)
 *   - Diaspora networks & international scholars: observers whose outside-the-frame analysis documents the constraint's contestability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.87).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.91).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections: Restrictive Reading (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'bc283cc8-9c6f-4b19-a826-6e854025b873').
narrative_ontology:cs_kernel_codification('bc283cc8-9c6f-4b19-a826-6e854025b873', fixed_text).
narrative_ontology:cs_authority_grounding('bc283cc8-9c6f-4b19-a826-6e854025b873', extraction).
narrative_ontology:cs_interpretation_layer_present('bc283cc8-9c6f-4b19-a826-6e854025b873').
narrative_ontology:cs_reading_relation('bc283cc8-9c6f-4b19-a826-6e854025b873', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('bc283cc8-9c6f-4b19-a826-6e854025b873', lausanne_minority_protections__guarantor_reading, forecloses).
narrative_ontology:cs_axiom('bc283cc8-9c6f-4b19-a826-6e854025b873', foundational, treaty_interpretation_domestic_authority).
narrative_ontology:cs_axiom_status(treaty_interpretation_domestic_authority, holdable).
narrative_ontology:cs_axiom_grounding('bc283cc8-9c6f-4b19-a826-6e854025b873', treaty_interpretation_domestic_authority, conventional).
narrative_ontology:cs_axiom('bc283cc8-9c6f-4b19-a826-6e854025b873', foundational, religious_liberty_individual_worship_only).
narrative_ontology:cs_axiom_status(religious_liberty_individual_worship_only, holdable).
narrative_ontology:cs_axiom_grounding('bc283cc8-9c6f-4b19-a826-6e854025b873', religious_liberty_individual_worship_only, deontological).
narrative_ontology:cs_axiom('bc283cc8-9c6f-4b19-a826-6e854025b873', secondary, institutional_pluralism_threat_to_sovereignty).
narrative_ontology:cs_axiom_status(institutional_pluralism_threat_to_sovereignty, overridden).
narrative_ontology:cs_axiom_grounding('bc283cc8-9c6f-4b19-a826-6e854025b873', institutional_pluralism_threat_to_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('bc283cc8-9c6f-4b19-a826-6e854025b873', turkish_domestic_legal_sovereignty).
narrative_ontology:cs_drift_state('bc283cc8-9c6f-4b19-a826-6e854025b873', contemporary_post_eu_integration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc283cc8-9c6f-4b19-a826-6e854025b873', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, greek_orthodox_minority).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_apostolic_minority).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, jewish_minority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, diaspora_advocacy_networks).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, diaspora_advocacy_networks).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, state_domestic_jurisdiction_over_religious_institutions).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, religious_liberty_as_individual_worship_only).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the restrictive interpretation of Lausanne protections. Administers denial of legal personhood to minority religious institutions, refuses theological school licensing, confiscates or controls property previously held by minority institutions, and subjects minority institutional governance to general Turkish law rather than international minority protections. Justifies these policies as protecting national sovereignty and secular governance. Controls the interpretive authority through domestic courts and state apparatus.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Denied legal personhood for the Ecumenical Patriarchate; cannot own property or sign contracts in institutional capacity. The Theological School of Halki (closed 1971) provided clergy education for over 100 years; its closure under Turkish law prohibiting theological seminaries forecloses institutional clergy training. Cannot exercise institutional autonomy or property rights. Religious identity (Orthodox Christianity) is constitutive of ethnic identity (Greek), making exit impossible. Faces confiscation of historic churches and religious property; must pursue legal claims as individuals rather than as institutional collective.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, greek_orthodox_minority, payer,
    powerless, generational, identity_locked, national).

% Denied legal personhood for the Armenian Apostolic Church; property formerly held by the Church is treated as unclaimed assets or Turkish state property. Theological education is prohibited; clergy must be trained outside Turkey or underground. Historical trauma (1915 genocide) is intertwined with Armenian institutional identity (the Church preserved Armenian identity through diaspora), making institutional suppression carry genocidal resonance. Identity fusion to Armenian Christianity makes exit unthinkable despite systematic institutional disempowerment. Property restitution claims are blocked by domestic law framing property disputes as civil matters, not minority protections.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_apostolic_minority, payer,
    powerless, generational, identity_locked, national).

% Retains higher legal status than Greek Orthodox or Armenian minorities due to Ottoman millet-system legacy and Turkish-Jewish diplomatic ties, but operates under the same restrictive reading framework. Jewish institutional property is subject to general Turkish law; religious community governance is subject to state oversight; educational autonomy is limited by secular curriculum requirements. Relative power provides some exit through diaspora business networks and international Jewish advocacy, but institutional autonomy remains constrained by Turkish domestic law interpretation of Lausanne.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, jewish_minority, payer,
    moderate, biographical, constrained, national).

% European Court of Human Rights (ECtHR) and European Commission for Human Rights would interpret Lausanne expansively and supervise minority protections through European human rights mechanisms. The restrictive reading forecloses their jurisdiction by asserting that Lausanne is interpreted through Turkish domestic law, not international law. They issued the Halki Seminary decision (1988) supporting expansive reading; Turkey refused compliance. Structurally excluded from the constraint's authority frame.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_human_rights_bodies, excluded,
    institutional, biographical, trapped, continental).

% Lausanne Treaty named France, Greece, and Britain as guarantor powers with supervisory responsibility for minority protections. The restrictive reading reframes Lausanne as a domestic matter, effectively excluding guarantor oversight. They retain formal standing in international law but lack enforcement mechanisms in modern contexts. Greece and Britain have raised minority protection concerns; Turkey asserts domestic sovereignty. Structurally excluded by the reading's authority-grounding.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states_france_greece_britain, excluded,
    powerful, generational, constrained, national).

% Analyze Lausanne treaty language and comparative minority-protection law from outside the Turkish domestic frame. Scholars document that Articles 39-44 mention 'religious communities,' 'institutions,' 'schools,' 'education,' and 'property' — language that expansive readings argue clearly protects institutional capacity. The restrictive reading must explain why treaty language grants these protections while domestic law restricts them. Serve as external check on the constraint's claimed naturalness and authority.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% Greek, Armenian, and Jewish diaspora organizations benefit from Turkey's restrictive policies: restrictive policies fuel advocacy legitimacy, donor support, and organizational importance. They benefit by being the external witnesses to Turkey's constraint. Simultaneously, they pay: they must absorb institutional functions (theological training, property management, community governance) that minorities in Turkey cannot exercise. Diaspora exists partly as consequence of the constraint; exit is geographic (they operate outside Turkey) but identity investment ties them to outcomes inside.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, diaspora_advocacy_networks, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, diaspora_advocacy_networks, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies which Lausanne protections bind Turkey domestically and which fall to international interpretation. By asserting individual-worship-only scope, the restrictive reading coordinates expectations: states know that minority institutional claims will not be enforced internationally; minorities know that institutional survival depends on state permission, not treaty rights.
% TRANSFER_FUNCTION: Transfers institutional capacity from minority religious communities to Turkish state apparatus: property ownership moves from religious institutions to state control or confiscation; educational authority over clergy moves from religious communities to state licensing authority; legal personhood for minority institutions is denied, forcing property and contract claims into individual rather than collective channels.
% ABSENT_VOICES: Guarantor states (France, Greece, Britain) are excluded by the reading's assertion of domestic authority. European human rights bodies are excluded by the same frame. Minority institutional leaders are not consulted in interpreting the protections nominally granted to them; their interpretation is foreclosed by the restrictive reading's definitional authority. Sibling readings (expansive, guarantor) represent perspectives excluded from the restrictive frame.
% DISAPPEARANCE_RATIONALE: If the restrictive reading disappeared, minority institutions would immediately restore theological schools, reassert property claims, reconstitute autonomous governance, and engage international guarantor and human rights mechanisms. The entire institutional ecology of the Greek Orthodox, Armenian Apostolic, and Jewish minorities would reorganize. The Turkish state would lose unilateral control over minority institutional capacity; minorities would recover institutional autonomy grounded in treaty language and international law.
% FOUNDING_PROBLEM: Manage religious pluralism and minority institutional autonomy within a newly formed Turkish nation-state consolidating secular governance and strong state sovereignty. In 1923, the Lausanne Treaty encoded protections for religious minorities; the founding problem was interpreting those protections in ways that did not fragment Turkish sovereignty or create autonomous enclaves.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and European human rights bodies attest the founding problem is substantially solved: modern nation-states manage religious minorities through secular law and human rights protections without requiring institutional fragmentation or sovereignty loss. Turkey itself demonstrates nation-state stability and state capacity independent of the level of minority institutional restriction. The Turkish state attests the problem remains live (invoking security and unity concerns), but these concerns are voiced by no external authority absent Turkey's assertion. The gap between internal (state: live) and external (scholarly/international: dead) attestation is the core signal: the constraint persists long after its founding problem is solved.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.87 at present) because the constraint systematically denies minority institutions the capacity to own property, educate clergy, retain legal personhood, or operate autonomously — all institutional functions are subordinated to state control. Suppression is even higher (0.91) because the constraint must be actively enforced: resistive minorities would otherwise claim the Lausanne protections that treaty text appears to grant; the state must continuously reinterpret, deny legal status, and close institutions that try to operate under alternative readings. Theater ratio is moderate-low (0.42) because the constraint's original justification (managing religious pluralism and state sovereignty) has been substantially solved by modern nation-state consolidation and secular law; much of the enforcement apparatus now serves purely extractive ends (property control, institutional foreclosure), not genuine security coordination. The measurement series show rising extractiveness and suppression from 1923 to 2024: as Turkish state capacity increased and minority resistance proved containable, the constraint tightened. The theater ratio rose sharply at 1971 (Halki Seminary closure) when the justification shifted from managing pluralism to enforcing secularism, then stabilized as enforcement became routine performance rather than crisis response. All three metrics are authored on a single shared time grid (1923, 1950, 1971, 1990, 2010, 2024) so the temporal patterns are coherent and the engine can detect the drift trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state apparatus and the minority institutions experience fundamentally different constraints. From the state's position, the restrictive reading clarifies sovereignty and prevents institutional fragmentation — it is a governance tool, domestically rational, justified by security concerns. From the minority position, it is a legal cage: the same words that appear to grant protection (Lausanne Articles 39-44) are reread as deferring all substantive protection to state discretion. The seat divergence is structural: the state controls the reading authority; minorities do not. The engine will compute radically different types from each seat: from the state's position, a coordination mechanism (rope or legitimate-governance scaffold); from the minority position, pure extraction (snare). This divergence is not an error — it is the signal that the constraint's claim (individual-worship-only protection) and the measured metrics (high extraction, high suppression, rising theater) are observing the same system from opposite seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus sits at the beneficiary end (d ≈ 0.05): the restrictive reading grants it unilateral interpretive authority over Lausanne, legal control over minority institutions, and systematic extraction of institutional capacity. Minority communities sit at the target end (d ≈ 0.95): they bear costs (property confiscation, educational foreclosure, legal disempowerment) and have trapped exit (especially Greek Orthodox and Armenian, locked by religious identity). The constraint concentrates extraction on the least-powerful seats (powerless minorities) at a large spatial scope (national), making effective extraction extremely high for those seats. European human rights bodies and guarantor states face a different directionality problem: they are formally excluded from the constraint's structure (the restrictive reading asserts Turkish domestic law is the frame), so d for them is undefined within the constraint itself — they are boundary actors. Diaspora networks have moderate d (they benefit from advocacy legitimacy; they pay by absorbing institutional functions that would normally occur inside Turkey), making them partially coordinated payers.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandate obsolescence is present. The founding problem (managing religious pluralism and minority fragmentation within a newly formed nation-state) has been substantially solved by modern nation-state consolidation, secular legal systems, and European integration. Most comparable nation-states manage religious minorities without denying institutional capacity or closing theological schools. Yet the restrictive reading persists and intensifies. The theater_ratio trajectory is diagnostic: rising from 0.08 (1923) to 0.42 (2024) suggests the constraint's enforcement increasingly serves performative ends (maintaining the restrictive-reading frame) rather than solving genuine coordination problems. The constraint is a piton-candidate: the founding problem is dead, but the administrative machinery persists because the state benefits from the extraction and because minorities face trapped exit. The measurement series support this: base_extractiveness and suppression_requirement rise monotonically over 100+ years after the founding problem is solved, indicating that enforcement is decoupled from any genuine protective need and driven by institutional capture. A genuine rope or scaffold would stabilize or decline as the original problem is resolved; this constraint tightens, which is the signature of extraction outlasting its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_vs_restrictive_reading,
    'Does the plain language of Lausanne Articles 39-44 constrain the restrictive reading, or is interpretation through Turkish domestic law an authorized frame that subordinates treaty text to national sovereignty?',
    'Comparative treaty interpretation scholarship; International Court of Justice advisory opinion on Lausanne minority-protection obligations; systematic comparison of how other signatory states interpret the same articles.',
    'If treaty text constrains, the restrictive reading violates international law; if domestic interpretation is authorized, the reading is legally defensible but politically contested. The constraint''s legal status turns on which frame is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_text_vs_restrictive_reading, conceptual, 'Whether treaty text or domestic interpretation law is the authoritative reading frame for Lausanne.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (minority fragmentation, sovereignty risk) still live in 2024, or has it been structurally solved by modern nation-state consolidation, European integration, and human rights law?',
    'Ethnographic study of minority stability and institutional viability under the restrictive reading vs. under expansive readings (natural experiment from EU member states with similar minorities); long-term institutional resilience data.',
    'If the problem is dead, the restrictive reading persists as pure extraction riding on an obsolete justification (piton candidate, high theater_ratio as performative security concern). If the problem is live, the extraction serves a genuine state stabilization function. This determines whether mandatrophy exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the constraint''s founding justification remains valid in contemporary context.').

omega_variable(
    identity_lock_mechanism,
    'Is the Greek Orthodox and Armenian Apostolic minority''s exit-option limitation (identity_locked) a structural feature of religious identity fusion, or is it a choice the minority could abandon if exit costs were lowered?',
    'Qualitative research on diaspora return migration patterns; interview studies with minority institutional leaders on hypothetical exit scenarios; historical comparison to cases where religious minorities reorganized outside originary geography.',
    'If identity fusion is structural, the minority''s suppression score accurately reflects internalized constraint. If exit is theoretically available but economically/socially expensive, the suppression score under-estimates the extractiveness because it treats choice-constrained populations as intrinsically committed. This affects whether the constraint qualifies as snare (depends on measured suppression being structural, not illusory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether minority institutional lock-in is intrinsic to religious identity or contingent on exit-cost structure.').

omega_variable(
    restrictive_reading_kernel_contest,
    'Which reading of Lausanne is the constraint''s kernel — the text itself, or the authoritative interpretation by Turkish courts and state apparatus?',
    'This is the committer-frame ambiguity: the restrictive reading INSTANTIATES one reading, not the kernel. The kernel (the treaty + historical practice + competing interpretations) exists independently. The reading that this constraint exemplifies (restrictive_reading) asserts that Turkish domestic law is the authoritative interpreter. Sibling readings assert that treaty text, guarantor state oversight, or European human rights bodies are authoritative. The contest is real; this constraint is ONE position in it, not the adjudicator.',
    'This omega documents that the constraint''s claimed naturalness (''this is what Lausanne says'') is itself contested. High uncertainty here is the engine''s first signal that the reading is not a discovered fact but a defended claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restrictive_reading_kernel_contest, conceptual, 'Kernel contest between restrictive, expansive, and guarantor readings of Lausanne minority protection authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_restrictive_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.08).
narrative_ontology:measurement_basis(lausanne_restrictive_tr_t1923, observed).
narrative_ontology:measurement(lausanne_restrictive_tr_t1950, lausanne_minority_protections__restrictive_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(lausanne_restrictive_tr_t1950, observed).
narrative_ontology:measurement(lausanne_restrictive_tr_t1971, lausanne_minority_protections__restrictive_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement_basis(lausanne_restrictive_tr_t1971, observed).
narrative_ontology:measurement(lausanne_restrictive_tr_t1990, lausanne_minority_protections__restrictive_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(lausanne_restrictive_tr_t1990, observed).
narrative_ontology:measurement(lausanne_restrictive_tr_t2010, lausanne_minority_protections__restrictive_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement_basis(lausanne_restrictive_tr_t2010, observed).
narrative_ontology:measurement(lausanne_restrictive_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(lausanne_restrictive_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(lausanne_restrictive_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.45).
narrative_ontology:measurement_basis(lausanne_restrictive_be_t1923, observed).
narrative_ontology:measurement(lausanne_restrictive_be_t1950, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement_basis(lausanne_restrictive_be_t1950, observed).
narrative_ontology:measurement(lausanne_restrictive_be_t1971, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement_basis(lausanne_restrictive_be_t1971, observed).
narrative_ontology:measurement(lausanne_restrictive_be_t1990, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1990, 0.79).
narrative_ontology:measurement_basis(lausanne_restrictive_be_t1990, observed).
narrative_ontology:measurement(lausanne_restrictive_be_t2010, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement_basis(lausanne_restrictive_be_t2010, observed).
narrative_ontology:measurement(lausanne_restrictive_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.87).
narrative_ontology:measurement_basis(lausanne_restrictive_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_restrictive_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.55).
narrative_ontology:measurement_basis(lausanne_restrictive_su_t1923, observed).
narrative_ontology:measurement(lausanne_restrictive_su_t1950, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(lausanne_restrictive_su_t1950, observed).
narrative_ontology:measurement(lausanne_restrictive_su_t1971, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1971, 0.79).
narrative_ontology:measurement_basis(lausanne_restrictive_su_t1971, observed).
narrative_ontology:measurement(lausanne_restrictive_su_t1990, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement_basis(lausanne_restrictive_su_t1990, observed).
narrative_ontology:measurement(lausanne_restrictive_su_t2010, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement_basis(lausanne_restrictive_su_t2010, observed).
narrative_ontology:measurement(lausanne_restrictive_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.91).
narrative_ontology:measurement_basis(lausanne_restrictive_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel decomposes into three structurally distinct constraints corresponding to three competing readings. The RESTRICTIVE READING (this file) interprets protections as individual-worship-only, placing institutions under general law — high extraction, snare-type. The EXPANSIVE READING interprets protections as guaranteeing functional institutional continuity — lower extraction, coordination-type. The GUARANTOR READING interprets protections as internationally supervised obligations enforceable through guarantor state mechanisms — structural escape from unilateral state interpretation. These readings do not represent measurement-basis differences on a single constraint; they represent genuinely different constraint structures with different beneficiary/victim sets, different ε values, and different extraction mechanisms. They are linked here because the contested kernel (Lausanne treaty) creates causal interdependence: adoption of expansive reading would directly foreclose restrictive reading within any single interpretive framework. The ε values differ across readings because the referent (the standing arrangement under contest from that reading's perspective) differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
