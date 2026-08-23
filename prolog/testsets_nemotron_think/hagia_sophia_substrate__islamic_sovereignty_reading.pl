% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Claim
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story models the Islamic sovereignty reading of the Hagia
 *   Sophia substrate kernel: the claim that the site's legitimacy derives
 *   exclusively from the 1453 Ottoman conquest and the continuous Islamic
 *   endowment (waqf) established thereafter, making it sovereign Islamic
 *   worship space under Turkish state authority. The 2020 reconversion
 *   (museum → mosque) via executive decree and Council of State ruling
 *   operationalized this reading as state-enforced constraint. The reading
 *   functions as a tangled rope: it coordinates worship, conservation, and
 *   access under a single authority (genuine coordination function) while
 *   extracting political capital for the AKP, symbolic victory for Turkish
 *   Islamists and the Sunni ummah, and sovereign control from UNESCO and
 *   Orthodox claimants (asymmetric extraction). The engine will compute
 *   per-seat classifications from the structural data authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.65).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.7).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, 'aad7a418-1752-4009-9da6-8a8d00689774').
narrative_ontology:cs_kernel_codification('aad7a418-1752-4009-9da6-8a8d00689774', fixed_text).
narrative_ontology:cs_authority_grounding('aad7a418-1752-4009-9da6-8a8d00689774', extraction).
narrative_ontology:cs_interpretation_layer_present('aad7a418-1752-4009-9da6-8a8d00689774').
narrative_ontology:cs_reading_relation('aad7a418-1752-4009-9da6-8a8d00689774', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('aad7a418-1752-4009-9da6-8a8d00689774', hagia_sophia_substrate__universal_heritage_reading, forecloses).
narrative_ontology:cs_axiom('aad7a418-1752-4009-9da6-8a8d00689774', foundational, ottoman_conquest_establishes_perpetual_islamic_sovereignty).
narrative_ontology:cs_axiom_status(ottoman_conquest_establishes_perpetual_islamic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('aad7a418-1752-4009-9da6-8a8d00689774', ottoman_conquest_establishes_perpetual_islamic_sovereignty, theological).
narrative_ontology:cs_axiom('aad7a418-1752-4009-9da6-8a8d00689774', foundational, turkish_state_inherits_waqf_authority).
narrative_ontology:cs_axiom_status(turkish_state_inherits_waqf_authority, holdable).
narrative_ontology:cs_axiom_grounding('aad7a418-1752-4009-9da6-8a8d00689774', turkish_state_inherits_waqf_authority, conventional).
narrative_ontology:cs_reference_frame('aad7a418-1752-4009-9da6-8a8d00689774', ottoman_waqf_sovereignty).
narrative_ontology:cs_drift_state('aad7a418-1752-4009-9da6-8a8d00689774', contemporary_akp_conversion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('aad7a418-1752-4009-9da6-8a8d00689774', '2026-08-03T14:22:11Z').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_apparatus).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, ottoman_waqf_perpetuity).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, conquest_confers_sovereignty).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, state_inherits_waqf_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiated and executed the 2020 reconversion via executive decree and court ruling. Gains political consolidation among religious-nationalist base, symbolic ownership of Ottoman legacy, and electoral mobilization from asserting sovereign Islamic authority over the site. Controls the enforcement apparatus.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, biographical, arbitrage, national).

% Receives the symbolic and religious satisfaction of restored Muslim worship at the historic site. The reconversion validates a core identity claim: that the Ottoman conquest established perpetual Islamic sovereignty. Exit from this beneficiary position would require abandoning the religious-nationalist identity framework that makes the claim meaningful.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, identity_locked, national).

% Gains a symbolic victory in the global Islamic imaginary: a Byzantine cathedral conquered and endowed as waqf, now reclaimed as sovereign Muslim space after a secular interregnum. No material transfer occurs, but the narrative of historical justice restored resonates across Sunni publics. Exit is identity-locked because the claim is woven into civilizational self-understanding.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    organized, civilizational, identity_locked, global).

% Face access restrictions (prayer times, gender segregation, carpeted prayer areas covering mosaics) and symbolic exclusion from a site that was a shared heritage monument. Christian visitors lose the ability to experience the building as a cathedral; secular tourists lose the neutral museum framing. Exit options are constrained: they can visit under Islamic terms or not visit.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    moderate, immediate, constrained, global).

% Loses jurisdictional authority over a World Heritage site. The 2020 conversion proceeded without UNESCO consent, violating the 1972 Convention's requirement for prior consultation. UNESCO's monitoring and advisory role is structurally denied; Turkish sovereignty is asserted as superior to international heritage governance. Exit is constrained because UNESCO cannot delist without political cost, but its authority is degraded.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime, payer,
    institutional, generational, constrained, global).

% Experience the reconversion as an ideological defeat: the 1934 museum conversion by Atatürk, symbol of the secular republic's reorientation toward universal heritage, is reversed. The site becomes a marker of the AKP's religious-nationalist project. Exit is constrained because the constraint is enforced by state power; dissent is politically risky and institutionally marginalized.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    organized, biographical, constrained, national).

% Provides the enforcement machinery: courts, police, Diyanet (Presidency of Religious Affairs) administration, security perimeter. Gains institutional coherence by aligning religious administration with executive authority. The Diyanet's control over the site extends its bureaucratic reach. Exit is arbitrage-grade: the state could reverse the policy, but the political cost would be enormous.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_apparatus, beneficiary).

% Holds the Orthodox restitution reading: the site was founded as a Christian cathedral and should return to ecclesiastical control or remain neutral. Has no legal standing in Turkish courts, no access to the site for worship, and no effective diplomatic leverage. Exit is trapped: the Patriarchate cannot leave Istanbul (its historic see), cannot compel Turkish compliance, and cannot abandon its claim without dissolving its own institutional identity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate, excluded,
    organized, civilizational, trapped, global).

% Scholars, NGOs, and heritage professionals who document the site's condition, monitor conservation standards, and analyze the precedent of unilateral conversion of World Heritage sites. They hold the universal_heritage_reading but lack enforcement power. Their role is analytical: producing evidence, framing discourse, and maintaining the universal heritage counter-narrative.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_heritage_community, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, undisputed sovereign authority (Turkish state via Diyanet) managing worship, conservation, and access at a historically contested site, preventing competing religious claims from generating perpetual conflict over physical control.
% TRANSFER_FUNCTION: Moves political legitimacy, religious symbolic capital, and control over heritage narrative from secular/universal frameworks to the AKP-Islamic coalition. The constraint transfers the site's global heritage status into a national-religious asset; the cost is borne by excluded claimants (Orthodox, UNESCO, secularists) and non-Muslim visitors who lose neutral access.
% ABSENT_VOICES: The Ecumenical Patriarchate (Orthodox restitution claim) is structurally excluded from Turkish decision-making. Armenian and Greek Orthodox communities with historical ties to the site have no consultative role. Local Istanbul residents who valued the museum's neutral civic space were not consulted. Their absence is maintained by Turkish sovereignty claims that treat foreign or minority religious authority as interference in domestic affairs.
% DISAPPEARANCE_RATIONALE: If the Islamic sovereignty claim vanished overnight, the site would revert to a contested vacuum: UNESCO would demand restoration of museum status or shared governance; the Ecumenical Patriarchate would press for Christian worship rights; the AKP would lose a signature achievement; Turkish secularists would mobilize for re-secularization; the Diyanet would lose administrative control. The physical site, its conservation regime, and its symbolic meaning would all be renegotiated.
% FOUNDING_PROBLEM: The 1453 conquest and subsequent waqf endowment established the site as an Islamic worship space in perpetuity; the 1934 secularization by Atatürk's regime was an illegitimate rupture that violated the waqf's terms and Islamic law. The constraint was built to solve the problem of restoring the site's true legal-religious status after an 86-year secular interregnum.
% FOUNDING_PROBLEM_CORROBORATION: The AKP and Diyanet attest the founding problem is live, citing waqf law and conquest sovereignty. The Turkish Council of State's 2020 ruling validated this reading. Corroboration from outside the beneficiary set: some international legal scholars argue waqf perpetuity is recognized in Ottoman/Turkish law; however, UNESCO, ICOMOS, the Ecumenical Patriarchate, Turkish secular opposition parties, and the Venice Commission of the Council of Europe attest the founding problem is either dead (the waqf was lawfully transformed) or contested (the 1934 conversion was a legitimate sovereign act). No neutral international body endorses the waqf-perpetuity claim as legally dispositive.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the constraint's dual character: real coordination of worship and site management coexists with significant political and symbolic extraction. The AKP gains electoral consolidation; Turkish Islamists gain identity validation; the Sunni ummah gains a civilizational symbol. These gains are real but not purely coordinative — they come at the expense of excluded claimants. Suppression (0.7) is high because the constraint's persistence depends on active state enforcement: court rulings, police security, Diyanet administration, and denial of UNESCO jurisdiction. Theater ratio (0.4) captures the performative dimension: the reconversion was staged as a historic restoration but functions substantially as political theater. Accessibility collapse (0.6) is moderate: alternative framings (museum, shared heritage, Orthodox cathedral) persist in global discourse but are structurally excluded from Turkish decision-space. Resistance (0.65) is substantial: UNESCO condemnation, Patriarchate protests, secularist opposition, and scholarly critique all contest the reading, but none have altered the enforcement reality.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (AKP, Turkish state), the constraint appears as legitimate restoration of waqf rights — a coordination function fulfilling a historical obligation. From the payer seats (non-Muslim visitors, UNESCO, secularists), the same structure operates as enforced extraction: sovereignty weaponized to exclude competing claims and consolidate political power. The Ecumenical Patriarchate experiences total foreclosure. The engine will compute this divergence from the power/exit/role declarations; the authored claimed_type (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition and Turkish state apparatus are structural beneficiaries (d ≈ 0.1-0.2): they control the constraint, collect its political-religious gains, and face arbitrage-grade exit (could reverse but won't). Turkish Islamic constituency and Sunni ummah are identity-locked beneficiaries (d ≈ 0.15-0.25): they gain symbolic capital but cannot exit the identity framework that makes the gain meaningful. Non-Muslim visitors, UNESCO, and secularist Turks are payers (d ≈ 0.7-0.9): they bear access restrictions, jurisdictional loss, and ideological defeat with constrained exit. The Ecumenical Patriarchate is trapped (d ≈ 0.95): no exit, no voice, structural exclusion. The international heritage community sits at analytical (d = 0.5): observes but neither collects nor pays.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate (restore waqf sovereignty) is contested as either live (waqf perpetuity never expired) or dead (1934 sovereign transformation was lawful). The mandate has not atrophied — it was actively revived in 2020 after 86 years. This is not a piton (inertial persistence) but a deliberate reactivation. The mandatrophy question is whether the coordination function (single-authority site management) could be served by a neutral framework that doesn't extract political-religious capital. The answer is contested: Turkey says no (sovereignty is indivisible); UNESCO says yes (shared governance models exist).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the Hagia Sophia substrate a single kernel with multiple legitimate readings, or are the islamic_sovereignty, orthodox_restitution, and universal_heritage readings structurally distinct constraints falsely unified by a shared label?',
    'Apply the ε-invariance test: if measuring the constraint''s extraction under the Islamic sovereignty reading yields a different ε than under the universal heritage reading (e.g., UNESCO jurisdiction vs. Turkish sovereignty), they are distinct constraints. Decompose into separate stories linked by network.affects_constraints.',
    'If distinct constraints, each gets its own ε, stakeholders, and classification. The islamic_sovereignty_reading would be tangled_rope; universal_heritage_reading might be rope or scaffold; orthodox_restitution_reading might be snare (extraction from Orthodox) or mountain (if treated as historical fact). The current story models only the Islamic sovereignty reading as a clean ε-invariant constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel label masks multiple ε-distinct constraints').

omega_variable(
    waqf_perpetuity_legal_status,
    'Does the 1453 waqf endowment retain binding legal force under modern Turkish and international law, or was it lawfully extinguished or transformed by the 1934 Council of Ministers decision?',
    'Comparative legal analysis of Ottoman waqf law, Turkish Republican succession, the 1934 decision''s legal form, and the 2020 Council of State ruling''s reasoning. Test: would a Turkish court in 1935 have accepted a waqf-beneficiary challenge to the museum conversion?',
    'If waqf perpetuity is legally binding, the islamic_sovereignty_reading''s coordination function is legally mandated (lowering extractiveness). If the 1934 conversion was lawful, the 2020 reconversion is a political choice dressed as legal restoration (raising extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_perpetuity_legal_status, empirical, 'Legal status of the waqf endowment across regime change').

omega_variable(
    political_consolidation_vs_religious_coordination,
    'How much of the constraint''s operation is genuine religious coordination (managing worship, conservation, access) versus political consolidation for the AKP (electoral signaling, nationalist-religious base mobilization)?',
    'Disaggregate the constraint''s activities: Diyanet administration of worship (coordination) vs. presidential ceremony rhetoric, election campaign usage, diplomatic signaling (extraction). Measure resource allocation: conservation budget vs. political communication budget.',
    'If political consolidation dominates, extractiveness is higher and the constraint trends toward snare. If religious coordination dominates, extractiveness is lower and the constraint is a purer tangled_rope. The current 0.65 extractiveness assumes substantial but not dominant political extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_consolidation_vs_religious_coordination, empirical, 'Disentangling coordination from political extraction in the constraint''s operation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative claims (UNESCO jurisdiction, Orthodox worship, secularist civic space) structural (state enforcement, legal barriers) or internalized (self-censorship, identity foreclosure, acceptance of Turkish sovereignty as final)?',
    'Post-exit trajectory analysis: if UNESCO or Patriarchate pressure increases after Turkish policy shifts, suppression is structural. If local secularist opposition persists despite repression, internalization is incomplete. Survey-based measurement of internalized foreclosure among Turkish secularists and minority communities.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint carries its own enforcement inside the subject. This would amplify χ for identity-locked payers (secularist Turks, Orthodox).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of excluded claimants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hs_islamic_sov_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hs_islamic_sov_tr_t30, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(hs_islamic_sov_tr_t60, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(hs_islamic_sov_tr_t86, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 86, 0.35).
narrative_ontology:measurement(hs_islamic_sov_tr_t87, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 87, 0.38).
narrative_ontology:measurement(hs_islamic_sov_tr_t90, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 90, 0.4).

% Extraction over time
narrative_ontology:measurement(hs_islamic_sov_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hs_islamic_sov_be_t30, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(hs_islamic_sov_be_t60, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(hs_islamic_sov_be_t86, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 86, 0.55).
narrative_ontology:measurement(hs_islamic_sov_be_t87, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 87, 0.62).
narrative_ontology:measurement(hs_islamic_sov_be_t90, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 90, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hs_islamic_sov_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hs_islamic_sov_su_t30, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(hs_islamic_sov_su_t60, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(hs_islamic_sov_su_t86, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 86, 0.6).
narrative_ontology:measurement(hs_islamic_sov_su_t87, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 87, 0.68).
narrative_ontology:measurement(hs_islamic_sov_su_t90, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 90, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the hagia_sophia_substrate constraint family. The islamic_sovereignty_reading asserts exclusive legitimacy from conquest/waqf; the universal_heritage_reading asserts transcendent shared heritage; the orthodox_restitution_reading asserts Byzantine Christian founding legitimacy. The three readings have different ε values (this reading: moderate-high ε; universal heritage: low ε; orthodox restitution: contested ε) and different beneficiary/victim structures. They are linked as sibling constraints because they compete for authority over the same physical site and legal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
