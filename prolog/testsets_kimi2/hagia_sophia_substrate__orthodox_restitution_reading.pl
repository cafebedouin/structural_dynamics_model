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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Orthodox Restitution Reading of Hagia Sophia Legitimacy
 *   domain: cultural heritage/sovereignty/religious authority
 *
 * SUMMARY:
 *   The Hagia Sophia substrate is a contested kernel of legitimacy claims
 *   over the Istanbul monument. This constraint story instantiates the
 *   orthodox_restitution_reading: the claim that the site's legitimacy
 *   derives from its 6th-century founding as a Christian cathedral under
 *   Justinian, generating a normative imperative that it should return to
 *   Orthodox ecclesiastical control or at minimum remain religiously neutral
 *   to honor its Byzantine origins. The constraint operates as a standing
 *   delegitimization of Turkish sovereignty over the site, extracting
 *   symbolic and diplomatic value for the Orthodox diaspora and the Greek
 *   state while imposing reputational and sovereignty costs on Turkey and
 *   insecurity on the Turkish Muslim worship community. Material extraction
 *   is lowâthere is no enforcement pathwayâbut symbolic generativity is
 *   high.
 *
 * KEY AGENTS:
 *   - orthodox_diaspora (organized/global): Primary symbolic beneficiary â derives identity cohesion and religious status from the claim
 *   - greek_state (institutional/national): Strategic beneficiary â deploys the claim as diplomatic leverage
 *   - turkish_state (institutional/national): Primary target â bears delegitimization of sovereignty over national territory
 *   - turkish_muslim_community (organized/national): Secondary target â bears insecurity about worship continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.38).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.42).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, snare).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Reading of Hagia Sophia Legitimacy").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural heritage/sovereignty/religious authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '689298af-577a-4cc3-b2bb-f61e60139828').
narrative_ontology:cs_kernel_codification('689298af-577a-4cc3-b2bb-f61e60139828', fixed_text).
narrative_ontology:cs_authority_grounding('689298af-577a-4cc3-b2bb-f61e60139828', lineage).
narrative_ontology:cs_interpretation_layer_present('689298af-577a-4cc3-b2bb-f61e60139828').
narrative_ontology:cs_reading_relation('689298af-577a-4cc3-b2bb-f61e60139828', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('689298af-577a-4cc3-b2bb-f61e60139828', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('689298af-577a-4cc3-b2bb-f61e60139828', foundational, byzantine_founding_confers_perpetual_ecclesiastical_status).
narrative_ontology:cs_axiom_status(byzantine_founding_confers_perpetual_ecclesiastical_status, holdable).
narrative_ontology:cs_axiom_grounding('689298af-577a-4cc3-b2bb-f61e60139828', byzantine_founding_confers_perpetual_ecclesiastical_status, theological).
narrative_ontology:cs_axiom('689298af-577a-4cc3-b2bb-f61e60139828', foundational, restitution_or_neutrality_as_historical_justice).
narrative_ontology:cs_axiom_status(restitution_or_neutrality_as_historical_justice, holdable).
narrative_ontology:cs_axiom_grounding('689298af-577a-4cc3-b2bb-f61e60139828', restitution_or_neutrality_as_historical_justice, deontological).
narrative_ontology:cs_reference_frame('689298af-577a-4cc3-b2bb-f61e60139828', byzantine_orthodox_founding).
narrative_ontology:cs_drift_state('689298af-577a-4cc3-b2bb-f61e60139828', contemporary_turkish_republic, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('689298af-577a-4cc3-b2bb-f61e60139828', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_muslim_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives symbolic religious and cultural identity from the site as the historic center of Eastern Orthodox Christianity. Commemorates the 1453 fall and ongoing status in liturgical memory and diaspora community discourse. Bears no direct material cost from the constraint's operation, but receives intangible status and cohesion benefits from its persistence as an active claim.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, orthodox_diaspora, beneficiary,
    organized, generational, mobile, global).

% Uses the restitution claim as diplomatic leverage in bilateral relations with Turkey and as a signal to domestic constituencies and co-religionists. The claim generates international attention and moral framing that can be activated or deactivated depending on geopolitical needs. Costs little to maintain while providing recurring symbolic capital.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, mobile, national).

% Bears the cost of persistent delegitimization of its sovereignty over the site. The Orthodox restitution claim frames Turkish administration as historically illegitimate occupation, imposing a recurring diplomatic and reputational burden in international forums and bilateral relations with Greece and Orthodox-majority states.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state, payer,
    institutional, generational, constrained, national).

% Experiences the restitution claim as a standing threat to Islamic worship continuity at the site. The claim's persistence in international discourse generates insecurity about the durability of current mosque status, particularly when activated by Greek or Orthodox institutional actors. Exit is constrained by religious obligation to the site and by the claim's existence outside their control.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_muslim_community, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Eastern Orthodox diasporic identity and Greek diplomatic signaling around a shared symbol of historical religious primacy, aligning transnational Orthodox communities and Hellenic foreign-policy rhetoric.
% TRANSFER_FUNCTION: Moves symbolic legitimacy and diplomatic leverage from Turkish sovereign control to Orthodox ecclesiastical and Greek state narratives; transfers intangible status to the Orthodox diaspora at the cost of Turkish sovereignty framing and Muslim worship security.
% ABSENT_VOICES: Turkish Cypriot Orthodox communities and ecumenical patriarchate critics who might seek liturgical use without political restitution are absent from the claim's framing; secular Turkish heritage professionals who might support museum status over both religious exclusivities are marginalized.
% DISAPPEARANCE_RATIONALE: If the restitution claim vanished, the Turkish state and Muslim community would experience reduced delegitimization pressure, while the Orthodox diaspora would lose a core mobilizing symbol and the Greek state would lose a diplomatic instrument. The material world would likely remain unchanged; the symbolic and diplomatic world would rearrange.
% FOUNDING_PROBLEM: The 1453 Ottoman conquest and subsequent conversion of Hagia Sophia from Christian cathedral to mosque represented a loss of Orthodox ecclesiastical control over a sacred site, creating an ongoing grievance about rightful religious and civilizational custodianship.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish Ministry of Foreign Affairs and domestic Muslim religious authorities attest from the target seat that the founding problem is historically settled. Independent international heritage bodies attest from an analytical seat that the problem is contested and best managed through heritage-status neutrality. No fully independent party outside all affected national and religious interests corroborates the claim as a live injustice requiring restitution.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.38, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.38 (low material, moderate symbolic): the constraint generates diplomatic friction and symbolic grievance but has no material enforcement pathway. Suppression at 0.42 reflects normative suppression of Turkish sovereignty narratives in certain international and religious forums, not physical coercion. Theater_ratio at 0.60 is elevated because the claim is sustained primarily through commemorative speech acts, anniversary politics, and diplomatic signaling rather than through functional implementation. Accessibility_collapse is low (0.25) because Turkish control remains the robust de facto alternative. Resistance is high (0.85) because the Turkish state and Muslim community actively reject the claim.
 *
 * PERSPECTIVAL GAP:
 *   From the Greek state seat, the constraint is a legitimate historical-justice claim protecting a sacred monument from illegitimate occupation. From the Turkish state seat, it is an irredentist nuisance that questions settled sovereignty. From the diaspora seat, it is a core identity anchor. From the Muslim community seat, it is a standing threat to religious practice. The engine computes these divergent types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Orthodox diaspora and Greek state are declared beneficiaries: they receive symbolic status and diplomatic leverage respectively, placing their directionality near the beneficiary pole (low d). The Turkish state and Turkish Muslim community are declared victims/payers: they bear sovereignty delegitimization and worship insecurity, placing their directionality near the target pole (high d). Effective extraction is thus amplified for the Turkish seats and damped for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by acknowledging that while the 1453 conquest was the founding grievance, the constraint's current function is not coordinated protection of heritage but asymmetric extraction of diplomatic leverage. The Greek state could abandon the claim with little material loss, while Turkey cannot unilaterally end the delegitimization. The absence of a sunset clause, active enforcement, or transitional justification rules out scaffold and rope. The high theater ratio and lack of material enforcement exclude tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historic_claim_or_diplomatic_instrument,
    'Is the Orthodox restitution claim a genuine theological-historical commitment, or primarily a diplomatic instrument deployable by the Greek state?',
    'Analyze activation patterns: if the claim is activated primarily during bilateral tensions and deactivated during cooperative phases, it functions as diplomatic instrument. If activation tracks liturgical calendars and religious commemoration independent of diplomatic utility, it functions as genuine commitment.',
    'If primarily diplomatic instrument, extraction is state-strategic rather than religious-symbolic, and the beneficiary set should weight the Greek state higher. If genuine commitment, the Orthodox diaspora''s symbolic benefit is primary and the constraint is more accurately an identity_coordination mechanism gone extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historic_claim_or_diplomatic_instrument, empirical, 'Whether the restitution claim is authentic religious commitment or diplomatic cover').

omega_variable(
    foreclosure_vs_coexistence,
    'Does the Orthodox restitution reading structurally foreclose the Islamic sovereignty reading, or do they coexist as competing geopolitical frames?',
    'Examine whether any international legal or diplomatic framework has successfully held both the Byzantine-founding legitimacy and the Ottoman-waqf legitimacy as simultaneously operative for the same site.',
    'If foreclosed, the kernel is a zero-sum sovereignty contest. If coexistent, the kernel permits synthetic or alternating-arrangement solutions (shared use, time-sharing, museum status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence, conceptual, 'Logical relationship between Orthodox and Islamic sovereignty readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_orthodox_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.7).
narrative_ontology:measurement(hagia_orthodox_tr_t20, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(hagia_orthodox_tr_t45, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 45, 0.7).
narrative_ontology:measurement(hagia_orthodox_tr_t70, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 70, 0.75).
narrative_ontology:measurement(hagia_orthodox_tr_t85, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 85, 0.8).
narrative_ontology:measurement(hagia_orthodox_tr_t86, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 86, 0.85).
narrative_ontology:measurement(hagia_orthodox_tr_t90, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 90, 0.75).

% Extraction over time
narrative_ontology:measurement(hagia_orthodox_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hagia_orthodox_be_t20, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(hagia_orthodox_be_t45, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 45, 0.22).
narrative_ontology:measurement(hagia_orthodox_be_t70, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 70, 0.3).
narrative_ontology:measurement(hagia_orthodox_be_t85, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 85, 0.35).
narrative_ontology:measurement(hagia_orthodox_be_t86, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 86, 0.5).
narrative_ontology:measurement(hagia_orthodox_be_t90, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 90, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, islamic_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hagia_sophia_substrate kernel, decomposed per the epsilon-invariance principle because the natural-language label 'Hagia Sophia legitimacy' conflates three structurally distinct claims: Orthodox restitution (this file), Islamic sovereignty, and universal heritage. Each reading has different beneficiaries, victims, and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
