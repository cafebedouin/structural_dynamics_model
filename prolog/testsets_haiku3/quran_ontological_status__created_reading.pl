% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq Reading)
 *   domain: theological/philosophical
 *
 * SUMMARY:
 *   The created reading (makhlūq) asserts that the Qur'an is God's created
 *   speech — temporally located, addressed to a specific historical audience
 *   — while God's essence transcends all temporal artifacts and remains
 *   eternally uncreated. This reading locates divine transcendence above the
 *   text itself, preserving God's absolute freedom while permitting human
 *   reason to investigate textual meaning. It is the reading instantiated by
 *   Muʿtazilite rational theology, adopted as state doctrine during the
 *   Abbasid mihna (9th century), and foundational to philosophical theology
 *   traditions. It benefits rationalist theologians, reform movements, and
 *   philosophical schools by granting them hermeneutic authority; it
 *   constrains traditionalist jurists and literalist communities whose
 *   authority derives from claiming that textual fixity is itself divine. The
 *   constraint is CLAIMED as rope (genuine coordination solving a real
 *   theological problem) while measured extractiveness reaches 0.68 at
 *   interval end — a substantial asymmetry the engine will detect. This is
 *   not a defect; the asymmetry models the reading's dual character:
 *   rationally coherent coordination framework AND asymmetric displacement of
 *   textual authority.
 *
 * KEY AGENTS:
 *   - rationalist_theologians: Beneficiaries (institutional power, arbitrage exit) — gain hermeneutic authority
 *   - traditionalist_jurists: Payers (powerful, constrained exit) — lose textual-fixity authority claim
 *   - literalist_communities: Payers (moderate power, identity-locked exit) — experience threat to identity-fusion with unmediated speech
 *   - reform_movements: Beneficiaries (organized power, mobile exit) — gain reinterpretive flexibility
 *   - philosophical_schools: Beneficiaries (organized power, mobile exit) — gain institutional legitimacy and patronage
 *   - state_authority: Agenda-setter (institutional, variable alignment) — may enforce or permit the reading
 *   - lay_believers: Dual-positioned (powerless, constrained exit) — gain coherent theodicy but lose directness of textual access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.68).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.42).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "theological/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, 'd864e133-41b3-4bcc-bc6f-802c677f04c1').
narrative_ontology:cs_kernel_codification('d864e133-41b3-4bcc-bc6f-802c677f04c1', formalized).
narrative_ontology:cs_authority_grounding('d864e133-41b3-4bcc-bc6f-802c677f04c1', lineage).
narrative_ontology:cs_interpretation_layer_present('d864e133-41b3-4bcc-bc6f-802c677f04c1').
narrative_ontology:cs_reading_relation('d864e133-41b3-4bcc-bc6f-802c677f04c1', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('d864e133-41b3-4bcc-bc6f-802c677f04c1', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('d864e133-41b3-4bcc-bc6f-802c677f04c1', foundational, divine_transcendence_above_text).
narrative_ontology:cs_axiom_status(divine_transcendence_above_text, holdable).
narrative_ontology:cs_axiom_grounding('d864e133-41b3-4bcc-bc6f-802c677f04c1', divine_transcendence_above_text, deontological).
narrative_ontology:cs_axiom('d864e133-41b3-4bcc-bc6f-802c677f04c1', foundational, rational_interpretation_legitimate).
narrative_ontology:cs_axiom_status(rational_interpretation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d864e133-41b3-4bcc-bc6f-802c677f04c1', rational_interpretation_legitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('d864e133-41b3-4bcc-bc6f-802c677f04c1', created_speech_with_transcendent_god).
narrative_ontology:cs_drift_state('d864e133-41b3-4bcc-bc6f-802c677f04c1', contemporary_pluralist_theo_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d864e133-41b3-4bcc-bc6f-802c677f04c1', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, lay_believers).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, lay_believers).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, divine_transcendence_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, rational_interpretation_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains hermeneutic and philosophical authority to interpret the Qur'an through rational theology (kalām). By classifying revelation as created temporal speech rather than eternal divine essence, they secure the logical space for human reasoning to mediate between God's transcendent being and textual meaning. Their interpretive frameworks become legitimate across reform and philosophical communities; their scholars shape educational curricula and legal reasoning. Exit would mean abandoning rational theology entirely.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    institutional, generational, arbitrage, regional).

% Loses the claim that the Qur'an's textual fixity is itself divine and inalienable. Under the created reading, their authority derives from interpretive consensus (ijmāʿ) and established jurisprudential schools rather than from unmediated access to divine speech. They bear the cost of competing with rationalist hermeneutics in educational and legal authority; their literal readings are reframed as one hermeneutic choice among many rather than as direct transmission of unchanging divine word. They can resist through institutional maintenance but cannot escape the philosophical challenge to their textual authority without abandoning jurisprudence entirely.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, constrained, regional).

% Experiences the reading as a threat to their identity as carriers of unmediated divine speech. Their self-understanding is constituted through a relationship to the Qur'an as God's direct word; if the Qur'an is created and mediated by rational interpretation, their role as preservers of something absolute and unchanging collapses. The cost is existential: they cannot adopt rational hermeneutics without dissolving the identity-fusion that makes them literalist communities. They may resist through institutional networks and community assertion, but the philosophical ground has shifted.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    moderate, biographical, identity_locked, regional).

% Gains flexibility to argue that revelation was contextualized to its historical moment (the created reading permits historical contingency of the text). Reform projects that depend on reinterpreting classical rulings in light of changed circumstances find philosophical cover: if the Qur'an is created speech addressing a specific historical audience, its rules can be rationally adapted. They benefit from the interpretive authority that rationalist theology provides.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    organized, biographical, mobile, regional).

% Gains institutional legitimacy and patronage for their rational-theological projects (Muʿtazilite, Mātūridī, later Ashʿarī syntheses). The created reading validates their core method: rational investigation (naẓar) of faith matters. They benefit from the hermeneutic framework the reading provides, becoming recognized as legitimate interpreters of revelation alongside jurists. They can exit by retreating to pure philosophy but would lose the theological authority the reading grants.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, regional).

% Gains the benefit of a coherent theodicy: if the Qur'an is created, God's essence remains free from temporal limitation and contradiction; believers can rationally defend God's justice (ʿadl). They also bear the cost of encountering their text through interpretive mediation rather than direct apprehension; scriptural authority becomes delegated to scholars and rational methods rather than self-evident. Their relationship to revelation becomes more intellectually sophisticated but less direct.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, lay_believers, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, lay_believers, payer).

% As institutional actors (Ḥanbalī leadership, traditionalist hadith schools), they are excluded from the rational-theological conversation or included only as interlocutors to be refuted. Their expertise in textual preservation and literal transmission becomes secondary to rational philosophy. They face institutional competition: reformed educational curricula increasingly privilege rational theology; patronage flows toward philosophical schools. They maintain institutional power through legal authority but lose cultural prestige in elite circles.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_scholars, excluded,
    powerful, generational, constrained, regional).

% Witness a theological framework that makes Islamic doctrine coherent within Greek rational categories. They contribute to the debate through philosophical disputation (Christian philosophers, Jewish philosophers, secular philosophers in later periods) and observe the constraint operate as rational method rather than as enforced doctrine. Their role is analytical; they neither collect from nor bear cost of the reading's operation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, non_muslim_philosophers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how finite human speech can legitimately represent infinite divine being. By classifying revelation as created (temporally located), the reading establishes rational theology as a coherent method for navigating revelation's textual meaning without collapsing God's transcendent essence into language. It enables multiple interpretive schools to coexist within a shared philosophical framework rather than claiming mutually exclusive literal truth.
% TRANSFER_FUNCTION: Moves hermeneutic and institutional authority from textual literalism and jurisprudential consensus toward rational theology and philosophical interpretation. Rationalist scholars, reform movements, and philosophical schools gain prestige, patronage, and educational authority; traditionalist jurists and literalist communities lose the claim that textual fixity itself is divine and unchanging, experiencing reframed authority as contingent on rational validation.
% ABSENT_VOICES: Uncreated-reading advocates (traditionalist and literalist communities at their most organized) would object that the created reading dissolves divine speech into merely temporal human meaning and displaces textual authority with rational ideology. They are partly excluded from the rational-theological conversation and partly included as defeated interlocutors. Lay believers seeking unmediated divine guidance are not organized enough to constitute a countervailing voice; they participate through their scholars rather than independently.
% DISAPPEARANCE_RATIONALE: If the created reading disappeared — if textual authority reverted entirely to literalism and uncreated-reading ontology — the institutional ecosystem would reorganize: rationalist theology would lose philosophical legitimacy, reform movements would lose hermeneutic cover for adaptation, philosophical schools would lose patronage, and traditionalist jurisprudence would reassert textual fixity as a primary authority claim. The legal and educational systems that incorporate rational theology would contract; scriptural interpretation would converge toward literal transmission rather than plural readings.
% FOUNDING_PROBLEM: How can a transcendent God communicate through temporal human language without either reducing God to temporal limitation or making the text opaque to human reason? The created reading solves this by asserting that revelation is God's created speech (temporally located, addressed to a specific audience) while God's being remains eternally uncreated and transcendent — preserving both divine freedom and rational intelligibility.
% FOUNDING_PROBLEM_CORROBORATION: Rationalist theologians, philosophical schools, and reform movements affirm the problem is live and the reading solves it coherently. Traditionalist jurists and literalist communities dispute whether the problem as framed is genuine; they attest that the uncreated reading preserves both transcendence and textual authority without rational mediation. Independent philosophical analysis (Averroist, medieval Christian scholastic engagement with Islamic theology, modern philosophy of religious language) attests the problem is structurally genuine and the created reading is a coherent solution, though contested.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__created_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the reading's operation depends on displacing textual authority from literalism to rational mediation — a genuine asymmetry where beneficiary seats (rationalists, reformers, philosophers) gain institutional prestige while payer seats (traditionalists, literalists) lose direct authority claims. The reading is not enforced by state power in this version (unlike the state_enforced_creation_reading sibling) — suppression stays moderate (0.42) because the constraint operates primarily through institutional competition and philosophical persuasion rather than through coercion. Theater is moderate (0.38) because some performative maintenance is required (elaborate defenses of rational method's legitimacy, ritual invocation of divine transcendence to cover the authority shift) but the underlying coordination function is genuine. The measurement series show extractiveness and suppression rising from t=0 to t=50 as rationalist theology becomes institutionalized (educational dominance, legal authority spread), then stabilizing — the constraint matures and reaches a steady state where the authority distribution is accepted by most institutional actors. Resistance stays high (0.71) because traditionalist jurisprudence maintains institutional power and literalist communities sustain identity-based opposition; the reading never achieves unanimous acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (rationalist theologians) experiences the constraint as genuine coordination: a coherent philosophical framework enabling legitimate interpretation of revelation in light of reason. The payer seats experience it as imposed authority: their established claims to textual truth are reframed as one interpretive choice among many, their institutional authority is diluted. The traditionalist jurist (powerful, constrained) experiences this asymmetry as a coordinated intellectual attack maintained by institutional competition; the literalist community (moderate power, identity-locked) experiences it as an existential threat they cannot escape. The reform movements experience it as liberation: hermeneutic flexibility they can use. The gap reflects genuine structural asymmetry, not merely disagreement about what is true — the created reading operates differently for different seats because it reshapes whose authority counts.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians sit at d ≈ 0.2 (strong beneficiaries): they gain hermeneutic authority, prestige, and institutional patronage; their exit options remain open (they could retreat to philosophy) but are rendered unnecessary. Traditionalist jurists sit at d ≈ 0.8 (near full targets): they experience authority reframing as loss; their constrained exit means they cannot abandon jurisprudence. Literalist communities sit at d ≈ 0.85 (full targets): identity-locked exit means the identity-fusion cost of the reading is existential; they cannot leave without dissolving what makes them literalist. Reform movements and philosophical schools sit at d ≈ 0.15 (beneficiaries): mobile exit means they could theoretically reject rational theology, but the benefits of institutional incorporation make that irrational. Lay believers sit at d ≈ 0.5 (symmetric): they gain theoretical coherence but lose interpretive directness; constrained exit traps them. The state_authority seat is analytical (d ≈ 0.5 by default) because whether they enforce the reading or permit it determines the constraint's type; in this version they permit it as one legitimate voice, so enforcement extraction is zero.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to preserve divine transcendence while permitting rational engagement with revelation — is live and genuinely solved by the created reading. This distinguishes the constraint from mandatroph (function decayed but persistence continues theatrically). However, the measurement series show extractiveness rising from t=0 to t=50 as institutional dominance accumulates, then stabilizing. This trajectory suggests two phases: (1) genuine coordination phase (t=0 to t=25) where the reading primarily solves the theological problem and institutional competition is open; (2) consolidation phase (t=25 to t=50) where rationalist theology achieves dominance and extractiveness increases because the authority asymmetry becomes institutionalized. After t=50, stabilization suggests the constraint has reached its steady state as rope: it persists through institutional maintenance, but the founding coordination function remains coherent. No full mandatroph signature appears — theater_ratio stays moderate rather than crossing 0.5 — but the measurement trajectory warns that if rationalist theology ever loses its philosophical coherence (if new challenges to theodicy emerge or rational method proves insufficient), the constraint could shift toward piton (maintained by institutional inertia). The high resistance measurement (0.71) indicates sustained ideological opposition that prevents the reading from achieving complete institutional entrenchment; this is protective against mandatroph drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_transcendence_ontology,
    'Does locating divine transcendence ''above'' created speech genuinely preserve God''s absolute freedom, or does it merely displace the problem of theodicy to a higher logical level?',
    'Sustained philosophical analysis of whether the created reading avoids the logical contradictions it claims to escape (particularly the problem of evil and divine justice) or merely relocates them. Comparison of theodicy coherence across created vs. uncreated readings in lived theological communities.',
    'If the created reading achieves genuine theodicy coherence while uncreated readings do not, it becomes more than coordination — it becomes a natural-law-adjacent constraint grounded in rational necessity. If both face equivalent philosophical problems, the reading''s authority is purely institutional and the extraction component rises. Type could shift from rope toward piton if coherence collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_transcendence_ontology, conceptual, 'Whether divine transcendence is actually preserved by the created reading or merely displaced.').

omega_variable(
    identity_lock_mechanism_literalists,
    'Is the literalist community''s experienced cost of the created reading truly existential (identity-fusion breaking), or can literalist identity persist under reinterpretation as one legitimate hermeneutic choice?',
    'Historical and ethnographic observation of literalist communities that encounter the created reading: do they reorganize identity around alternative forms of textual authority (e.g., hadith authentication, fiqh methodology) or does identity dissolution occur? Do second-generation literalists born into a created-reading context experience identity threat differently?',
    'If identity can reorganize without dissolution, the payer cost falls from existential (d=0.85) to biographical (d≈0.6), shifting directionality and potentially lowering extracted asymmetry. If identity truly cannot persist under reinterpretation, the extraction measurement is accurate and the constraint asymmetry is irreversible without the uncreated reading''s return.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_literalists, empirical, 'Whether literalist identity-lock can reorganize under the created reading or dissolves.').

omega_variable(
    rational_theology_as_cover,
    'Is rationalist theology''s institutional prestige earned through genuine philosophical coherence, or is it sustained partly by performative maintenance — elaboration of method and defense against critique — that would collapse if scrutiny intensified?',
    'Comparative analysis of rationalist theological literature at different historical moments: does the volume and intensity of defense against critique (theater activity) correlate with genuine philosophical problem-solving or with threat management? Do external philosophical challenges produce substantive methodological innovation or rhetorical elaboration?',
    'High theater_ratio (>0.5) would indicate performative maintenance and suggest piton drift; moderate theater ratio (0.38 measured) indicates some performative activity but genuine function persists. If theater increases above 0.45 while extractiveness plateaus, mandatroph diagnosis becomes more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_theology_as_cover, empirical, 'Whether rational theology''s institutional authority is grounded in philosophical coherence or partly sustained by performative maintenance.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the created reading logically foreclose the uncreated reading, or do both readings remain live options that different theological traditions hold simultaneously?',
    'Systematic analysis of whether an adherent to the uncreated reading can acknowledge the logical structure of the created reading and rationally choose uncreated reading anyway (coexistence) or whether accepting created reading''s premises entails logical commitment to rejecting uncreated reading''s core claim (foreclosure).',
    'If foreclosure: the created reading eliminates the uncreated reading''s rational defensibility; theological history should show uncreated reading abandoned by sophisticated thinkers. If coexistence: both remain rationally live; theological history should show capable thinkers holding both in different contexts. Current theology shows coexistence (Ashʿarism synthesis, later Sunni ecumenicism); this suggests coexists_with is the correct reading_relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the created and uncreated readings are logically incompatible (foreclosure) or can coexist as live positions.').

omega_variable(
    kernel_status_contested,
    'Is the ontological status of the Qur''an genuinely undetermined (multiple readings remain rationally defensible) or has one reading achieved sufficient philosophical dominance that the contest is settled within elite theological discourse?',
    'Survey of contemporary Islamic theological literature: do living theologians argue for the uncreated reading as rationally live, or only as historically authoritative? Do major universities teach both readings as legitimate options or as one dominant reading with uncreated as ''traditionalist'' background?',
    'If genuinely contested: the kernel remains open, multiple readings are live, and this constraint operates as rope within a contested ecology. If one reading achieves dominance: the kernel begins to close, the non-dominant reading becomes peripheral, and the dominant reading shifts toward mountain (natural-law-adjacent). Current state (measured at t=50 interval) suggests contest is live but dominance is consolidating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_status_contested, empirical, 'Whether the ontological status of the Qur''an remains genuinely contested or one reading has achieved philosophical dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__created_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(qura_tr_t12, observed).
narrative_ontology:measurement(qura_tr_t25, quran_ontological_status__created_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(qura_tr_t25, observed).
narrative_ontology:measurement(qura_tr_t38, quran_ontological_status__created_reading, theater_ratio, 38, 0.37).
narrative_ontology:measurement_basis(qura_tr_t38, observed).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(qura_tr_t50, observed).
narrative_ontology:measurement(qura_tr_t75, quran_ontological_status__created_reading, theater_ratio, 75, 0.36).
narrative_ontology:measurement_basis(qura_tr_t75, projected).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(qura_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__created_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(qura_be_t12, observed).
narrative_ontology:measurement(qura_be_t25, quran_ontological_status__created_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(qura_be_t25, observed).
narrative_ontology:measurement(qura_be_t38, quran_ontological_status__created_reading, base_extractiveness, 38, 0.62).
narrative_ontology:measurement_basis(qura_be_t38, observed).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(qura_be_t50, observed).
narrative_ontology:measurement(qura_be_t75, quran_ontological_status__created_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement_basis(qura_be_t75, projected).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(qura_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__created_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement_basis(qura_su_t12, observed).
narrative_ontology:measurement(qura_su_t25, quran_ontological_status__created_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(qura_su_t25, observed).
narrative_ontology:measurement(qura_su_t38, quran_ontological_status__created_reading, suppression_requirement, 38, 0.38).
narrative_ontology:measurement_basis(qura_su_t38, observed).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(qura_su_t50, observed).
narrative_ontology:measurement(qura_su_t75, quran_ontological_status__created_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(qura_su_t75, projected).
narrative_ontology:measurement(qura_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement_basis(qura_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The quran_ontological_status kernel comprises three constraint stories: (1) created_reading (this file) — rope classification, philosophical coordination, institutional authority shift; (2) uncreated_reading — mountain classification, textual fixity as natural law, traditionalist authority maintained; (3) state_enforced_creation_reading — tangled_rope or snare classification, same philosophical reading as created_reading but with state coercion enforcement, producing extraction rather than coordination. The created_reading influences both siblings: it establishes rational theology as a defensible position (influences uncreated_reading's rational challenge), and it provides the doctrinal content that state_enforced_creation_reading weaponizes (influences enforcement escalation). The three stories are linked by affects_constraints and represent decomposition of the natural-language 'Qur'an ontology debate' into structurally distinct constraints per the epsilon-invariance principle (OQ-69).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
