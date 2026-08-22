% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia Orthodox Restitution Legitimacy Claim
 *   domain: cultural_heritage/religious_authority/sovereignty
 *
 * SUMMARY:
 *   The Hagia Sophia stands as a focal point for contested historical
 *   legitimacy. This constraint story instantiates the Orthodox restitution
 *   reading: the claim that the site's foundational status as a Byzantine
 *   Orthodox cathedral creates an enduring normative entitlement—either to
 *   direct return to Orthodox ecclesiastical control or at minimum to
 *   neutrality that honors the Christian founding against exclusive Islamic
 *   worship use. This reading is one of three structurally distinct
 *   constraints all grounded in the same physical site (kernel) but
 *   authorizing legitimacy through different historical narratives. The
 *   restitution reading benefits the Eastern Orthodox diaspora (symbolically
 *   anchoring their identity) and the Greek state (as diplomatic leverage);
 *   it victimizes Turkish state sovereignty (as an external delegitimizing
 *   claim) and Islamic worship continuity (as a recurring threat to
 *   established practice). The constraint's extractiveness is low in material
 *   terms—no realistic enforcement pathway exists—but symbolically generative
 *   in geopolitical tension. Theater ratio is high (0.67): the claim operates
 *   almost entirely through narrative performance (historical revisiting,
 *   diplomatic rhetoric, identity affirmation) rather than material
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Eastern Orthodox diaspora: symbolic beneficiary; identity-locked to the claim; cannot exit without abandoning core heritage narrative
 *   - Greek state: institutional beneficiary; mobile exit but strategically committed to the claim as diplomatic resource
 *   - Turkish state sovereignty: payer (non-agent); trapped; faces external delegitimization challenge to territorial authority
 *   - Islamic worship continuity: payer (non-agent); trapped; faces latent legal and legitimacy interruption
 *   - Byzantine historical scholarship: observer seat; produces foundational-narrative evidence; analytically positioned
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.31).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.18).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, snare).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia Orthodox Restitution Legitimacy Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/religious_authority/sovereignty").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '52e1861d-7eac-429e-9d17-a00995a0dcd6').
narrative_ontology:cs_kernel_codification('52e1861d-7eac-429e-9d17-a00995a0dcd6', fixed_text).
narrative_ontology:cs_authority_grounding('52e1861d-7eac-429e-9d17-a00995a0dcd6', lineage).
narrative_ontology:cs_interpretation_layer_present('52e1861d-7eac-429e-9d17-a00995a0dcd6').
narrative_ontology:cs_reading_relation('52e1861d-7eac-429e-9d17-a00995a0dcd6', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('52e1861d-7eac-429e-9d17-a00995a0dcd6', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('52e1861d-7eac-429e-9d17-a00995a0dcd6', foundational, foundational_christian_purpose_creates_enduring_entitlement).
narrative_ontology:cs_axiom_status(foundational_christian_purpose_creates_enduring_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('52e1861d-7eac-429e-9d17-a00995a0dcd6', foundational_christian_purpose_creates_enduring_entitlement, deontological).
narrative_ontology:cs_axiom('52e1861d-7eac-429e-9d17-a00995a0dcd6', secondary, orthodox_restitution_honors_continuity_against_exclusion).
narrative_ontology:cs_axiom_status(orthodox_restitution_honors_continuity_against_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('52e1861d-7eac-429e-9d17-a00995a0dcd6', orthodox_restitution_honors_continuity_against_exclusion, instrumental).
narrative_ontology:cs_reference_frame('52e1861d-7eac-429e-9d17-a00995a0dcd6', byzantine_orthodox_foundational_authority).
narrative_ontology:cs_drift_state('52e1861d-7eac-429e-9d17-a00995a0dcd6', contemporary_post_ottoman_recovery_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('52e1861d-7eac-429e-9d17-a00995a0dcd6', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the symbolic claim that Hagia Sophia's foundational Christian identity creates an Orthodox claim to restitution or neutrality. This claim affirms their own heritage legitimacy and positions them as historical rightful voices in a site they cannot physically control. Exit from this claim would mean abandoning a core identity anchor—the Conquest and subsequent loss frame their diaspora identity itself.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    moderate, generational, identity_locked, global).

% Derives diplomatic and cultural leverage from the restitution claim in Turkish-Greek relations. The claim is a tool in broader geopolitical positioning, sanctioning pressure, and nationalist narrative. The state can modulate its commitment to the claim (as demonstrated by historical shifts in official rhetoric) but benefits from keeping it alive as a rhetorical resource.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, mobile, national).

% Bears the external legitimacy claim challenging its de facto and de jure control. The restitution reading asserts Turkey's sovereignty over the site is contingent on a prior Orthodox claim—a structural attack on territorial legitimacy. Turkey cannot exit this claim without abandoning sovereignty doctrine; it can only defend against it through institutional and narrative means.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty).

% Faces intermittent interruption or threat from the restitution claim whenever the Greek narrative gains diplomatic salience. The 2020 shift to mosque status was itself presented by Turkish authorities as vindicating the original conquest claim against this exact restitution pressure. This reading's persistence keeps Islamic worship under latent legal/legitimacy challenge.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).

% Would fiercely advocate for this reading if formally included in deliberations about the site's status, but are structurally excluded from direct negotiation—the claim operates through state diplomacy and diaspora identity maintenance, not through public Greek voice at the site itself.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_nationalist_public, excluded,
    powerful, biographical, identity_locked, regional).

% Is formally excluded from the restitution deliberation but carries the constraint's effects: the claim implicitly delegitimizes their worship at the site by asserting prior Christian foundational claims. Their stake in Islamic continuity is real but their voice is structurally absent from the legitimacy contest.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_islamic_public, excluded,
    powerful, biographical, identity_locked, regional).

% Monitor the restitution claim as evidence of cultural-nationalist overreach and religious exclusivity in heritage claims. They produce counter-analyses emphasizing shared ownership and the risk that any single-tradition restitution precedent erodes universal-heritage authority. They do not decide outcomes but document the legitimacy contest.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, international_heritage_advocates, observer,
    organized, biographical, analytical, global).

% Produces evidence about the site's founding, theological status, architectural intent, and historical use. This scholarly seat does not advocate for any reading but provides the empirical foundation on which all three readings stake their interpretations. The restitution reading cites foundational-Christian-purpose claims from this seat.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_historical_scholarship, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a normative framework for determining legitimate control over culturally-layered sites: the reading asserts that founding religious identity creates an enduring claim to restitution or at minimum non-exclusionary status, solving the problem of how to adjudicate multi-use religious heritage.
% TRANSFER_FUNCTION: Transfers legitimacy status from the current Islamic/Turkish authority structure to (at minimum) a neutral or (at maximum) an Orthodox restitution arrangement. No material transfer occurs under the current constraint; the transfer is symbolic/diplomatic—claims on the site's interpretation and on Turkey's geopolitical standing.
% ABSENT_VOICES: Turkish Islamic worship practitioners are structurally excluded from the restitution deliberation (they face the claim but do not author it); Turkish secular publics skeptical of religious framing are absent; contemporary Greek Orthodox practitioners in Turkey face suppression and cannot voice support for the claim on Turkish soil; global Muslim heritage advocates who would contest the restitution reading are absent from Greek-Turkish state negotiations.
% DISAPPEARANCE_RATIONALE: If the Orthodox restitution claim disappeared, Greek-Turkish diplomatic tension would persist but lose one major symbolic weapon, reducing NATO-alliance friction and potentially opening space for shared-heritage framings. Turkish state legitimacy would no longer face the external sovereignty challenge this reading represents. However, the deeper question of whose founding legitimacy 'counts' for multi-tradition sites would remain contested—disappearance of THIS reading would not dissolve the underlying contest, only one framing of it.
% FOUNDING_PROBLEM: The Hagia Sophia was founded as a Christian cathedral in 537 CE under Byzantine/Orthodox authority; it was converted to Islamic use following the 1453 Ottoman conquest. This reading asserts the foundational Orthodox purpose creates an enduring normative claim—either for direct restitution to Orthodox control or for neutrality that honors the Byzantine origins and prevents exclusive Islamic monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Byzantine historians and architectural scholars attest the site was founded as an Orthodox cathedral with explicit theological purpose. The Greek state asserts the founding problem persists as unresolved historical injustice. Turkish authorities and Islamic heritage advocates attest the founding problem has been decisively settled by 570 years of continuous Islamic stewardship and formal Ottoman waqf endowment. International heritage scholars from outside all parties attest the founding problem is a false choice that ignores the site's multi-tradition layering. No corroboration from Turkish institutional voices supports the restitution reading's claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.31 (low-moderate) because the claim transfers symbolic legitimacy and diplomatic standing but has no mechanism for material extraction or territorial reversal given Turkish de facto and de jure control since 1453. The measurement trajectory shows gradual increase from t=0 (0.18) to t=30 (0.31), reflecting intensifying geopolitical salience of the restitution claim (2020 mosque conversion, 2023 UNESCO statements, recurring Greek-Turkish diplomatic cycles). Theater ratio is high (0.67 at endpoint, rising from 0.45 at t=0) because the constraint operates almost entirely through legitimacy narrative and identity performance—no institutional enforcement machinery exists; only Greek diplomatic rhetoric, diaspora identity maintenance, and Turkish counter-assertions. Suppression is low (0.18) because the constraint encounters substantial resistance (0.78) and no coercive apparatus enforces it; Turkish state authority can suppress its voice in Turkish territory but cannot suppress the claim's circulation globally. The constraint is classified as snare because: (1) it identifies victims (Turkish sovereignty, Islamic continuity); (2) it persists through extracting symbolic advantage from a contestation rather than solving a coordination problem the payers voluntarily entered; (3) accessibility to alternatives is partly collapsed (the restitution framing has become institutionalized in Greek policy and diaspora identity) but substantial resistance and external authority contestation remain. The claim/metric gap is structural to the kernel-reading frame: from the Orthodox restitution reading's own epistemic position, the founding legitimacy claim is binding and continuous; from Turkish and Islamic seats the same claim is an externally-imposed delegitimization mechanism. The engine should compute different types from different seats.
 *
 * PERSPECTIVAL GAP:
 *   From the Eastern Orthodox diaspora and Greek state seat, this constraint resolves a long-standing historical injustice and honors foundational legitimacy—they perceive it as rooted in historical truth and moral entitlement. From the Turkish state and Islamic worship seat, it is a recurring external attempt to undermine national sovereignty and interrupt established religious practice—they perceive it as nationalist revisionism masquerading as historical rectification. From the international heritage seat it is an example of single-tradition restitionism that erases the site's multi-use reality and creates dangerous precedent for cultural nationalism globally. The engine computes per-seat classifications from these structural positions; the divergence is the measurement the system takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern Orthodox diaspora sits at low directionality (d ≈ 0.20–0.35): benefits from the claim (symbolic validation, identity affirmation, diaspora cohesion) without bearing its direct costs. Greek state sits near-beneficiary (d ≈ 0.25–0.40): collects diplomatic leverage and nationalist narrative benefit; can modulate commitment; moderate institutional power gives it mobile rather than trapped exit. Turkish state sovereignty is the full target (d ≈ 0.85–1.0): the claim directly challenges its territorial legitimacy, forces defensive counter-narration, and creates persistent NATO-friction; trapped exit (cannot abandon sovereignty doctrine). Islamic worship continuity is similarly targeted (d ≈ 0.80–0.95): faces recurring interruption threat; trapped in the same geopolitical structure. The directionality overrides are minimal here because beneficiary/victim + exit options derivation tracks the structural relationship accurately—the Orthodox and Greek seats accrue benefit, the Turkish and Islamic seats bear cost, and exit mobility differentiates the institutional from the symbolically-locked actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Byzantine Orthodox foundational purpose) was live in 1453 and dead thereafter (Ottoman conquest followed by 570 years of Islamic administration and waqf endowment established a new legitimacy structure). The restitution reading resurrects the dead founding problem and treats it as continuously live—a classic mandatrophy signature. The constraint persists not because the founding coordination need (Orthodox preservation of the site) remains unmet, but because the Greek state and Orthodox diaspora collect legitimacy benefit from keeping the claim alive. The disappearance_verdict is contested (not unanimous) because Turkish and Islamic voices deny the founding problem's current significance while Greek and Orthodox voices affirm it. This mismatch—dead founding problem + contested disappearance verdict—signals a constraint that has outlived its original justification and now operates as symbolic extraction. The mandate has shifted from solving the original coordination problem (preserve Byzantine Christianity) to solving a different one: maintain Greek-Orthodox cultural legitimacy claims in a post-Ottoman geopolitical context. These are not the same problem, which is why mandatrophy detection fires here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_legitimacy_persistence,
    'Does foundational religious purpose (Byzantine Christian origin in 537) create an enduring normative entitlement that survives 570+ years of continuous alternative use and institutional reconstitution?',
    'Comparative jurisprudence on cultural restitution precedents; philosophical debate on historical legitimacy decay; examination of whether foundational-narrative claims are applied consistently across similar cases (e.g., Al-Aqsa/Temple Mount, Canterbury Cathedral if it had been used as a mosque for centuries) or selectively.',
    'If foundational purpose is held to create enduring entitlement, the Orthodox restitution claim gains structural legitimacy and the constraint becomes a genuine coordinate-competing demand; if foundational purpose decays with time and alternative legitimacy accrues through continuous use, the claim becomes purely symbolic revanchism and the constraint reclassifies toward pure snare extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_legitimacy_persistence, preference, 'Whether founding legitimacy persists or decays under centuries of alternative use.').

omega_variable(
    enforcement_pathway_implausibility,
    'Is the Orthodox restitution claim materially implementable given Turkish sovereignty, NATO alliance constraints, and 600+ years of institutional continuity, or does it operate exclusively as symbolic/diplomatic leverage with zero realistic enforcement pathway?',
    'Assessment of enforcement capacity: does any coalition (Greek state, EU, Orthodox diaspora, international heritage advocates) command sufficient power and willingness to enforce restitution? Examination of historical precedent for religious site restitution against sovereign state authority (rare: Acropolis museums, disputed; Taj Mahal, no; Temple Mount, no; Canterbury post-hypothetical conquest, untested).',
    'If implementable, the constraint operates as latent snare with material extraction risk; if wholly implausible, theater_ratio is accurately high (0.67) and the constraint is pure symbolic performance, approaching piton classification (maintained through narrative rather than function, low extraction, high theater). This resolves into snare vs. piton type divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_pathway_implausibility, empirical, 'Whether restitution has any realistic enforcement pathway or operates purely symbolically.').

omega_variable(
    internalized_identity_lock_versus_strategic_choice,
    'For the Eastern Orthodox diaspora, is the restitution claim an internalized identity anchor that they cannot exit (true identity_locked exit), or is it a strategic narrative choice they could abandon if incentivized?',
    'Post-resolution trajectory: if a negotiated compromise (e.g., shared museum status, Orthodox prayer rights, neutral governance) dissolved the restitution demand and diaspora communities adopted it, the exit was mobile-strategic; if diaspora resist compromise and reformulate the claim, exit is identity_locked.',
    'If identity_locked, the Orthodox diaspora''s directionality is genuinely trapped at d ≈ 0.2–0.3 (beneficiary but unable to exit even if cost-benefit shifted); if strategic-mobile, d should be higher (0.4–0.5), indicating calculated choice rather than identity fusion. This affects per-seat classification from the diaspora seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_identity_lock_versus_strategic_choice, empirical, 'Whether Orthodox diaspora embrace of the restitution claim is an internalized identity or a strategic choice.').

omega_variable(
    kernel_codification_ambiguity,
    'Is the Hagia Sophia legitimacy kernel a fixed historical text (the founding as Orthodox cathedral), a distributed interpretive arena (each reading constructs the kernel differently), or implicit (no kernel exists—just competing present-day claims)?',
    'Examination of whether all three readings agree on the site''s foundational facts (architecture, date, initial purpose) or whether foundational facts themselves are contested. If facts align but interpretation diverges, kernel is fixed_text; if facts are contested, kernel is distributed or implicit.',
    'If fixed_text, the restitution reading can claim to be merely unpacking what the kernel says; if distributed or implicit, the restitution reading is revealed as constructing legitimacy rather than discovering it. This routes into the cs_structure choice and affects the plausibility of the reading''s own authority grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_ambiguity, empirical, 'Whether the Hagia Sophia founding narrative is a fixed historical fact or a distributed interpretive arena.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(hagi_tr_t0, observed).
narrative_ontology:measurement(hagi_tr_t5, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(hagi_tr_t5, observed).
narrative_ontology:measurement(hagi_tr_t10, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(hagi_tr_t10, observed).
narrative_ontology:measurement(hagi_tr_t15, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 15, 0.63).
narrative_ontology:measurement_basis(hagi_tr_t15, observed).
narrative_ontology:measurement(hagi_tr_t20, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement_basis(hagi_tr_t20, projected).
narrative_ontology:measurement(hagi_tr_t25, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 25, 0.66).
narrative_ontology:measurement_basis(hagi_tr_t25, projected).
narrative_ontology:measurement(hagi_tr_t30, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 30, 0.67).
narrative_ontology:measurement_basis(hagi_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(hagi_be_t0, observed).
narrative_ontology:measurement(hagi_be_t5, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(hagi_be_t5, observed).
narrative_ontology:measurement(hagi_be_t10, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement_basis(hagi_be_t10, observed).
narrative_ontology:measurement(hagi_be_t15, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement_basis(hagi_be_t15, observed).
narrative_ontology:measurement(hagi_be_t20, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(hagi_be_t20, projected).
narrative_ontology:measurement(hagi_be_t25, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(hagi_be_t25, projected).
narrative_ontology:measurement(hagi_be_t30, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(hagi_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(hagi_su_t0, observed).
narrative_ontology:measurement(hagi_su_t5, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement_basis(hagi_su_t5, observed).
narrative_ontology:measurement(hagi_su_t10, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement_basis(hagi_su_t10, observed).
narrative_ontology:measurement(hagi_su_t15, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement_basis(hagi_su_t15, observed).
narrative_ontology:measurement(hagi_su_t20, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement_basis(hagi_su_t20, projected).
narrative_ontology:measurement(hagi_su_t25, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement_basis(hagi_su_t25, projected).
narrative_ontology:measurement(hagi_su_t30, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(hagi_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.1).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% Hagia Sophia legitimacy kernel decomposes into three structurally distinct constraints: (1) Orthodox restitution reading—grounds legitimacy in founding Byzantine identity, asserts enduring entitlement to restitution or neutrality; (2) Islamic sovereignty reading—grounds legitimacy in 1453 conquest and continuous waqf administration, asserts Turkish/Islamic authority as settled fact; (3) Universal heritage reading—rejects founding-narrative legitimacy, asserts site belongs to all humanity. Each reading carries a different ε, different beneficiary/victim structure, and different type classification. All three coexist as live positions in Greek-Turkish diplomacy and global heritage discourse. The restitution reading influences the universal heritage reading (by forcing heritage advocates to defend against identity-exclusivity claims) and coexists with the Islamic sovereignty reading (they are held by different state/religious parties with no logical foreclosure between them). Ε values differ because the observables change: restitution measures extraction as delegitimization of Turkish sovereignty; Islamic sovereignty measures extraction as interruption of Islamic worship; universal heritage measures extraction as identity-nationalism overreach. No single observable yields coherent ε for all three; the decomposition into three stories preserves ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
