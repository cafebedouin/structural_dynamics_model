% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Claim
 *   domain: cultural/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The Hagia Sophia in Istanbul was converted from a museum back to a mosque
 *   by Turkish presidential decree in 2020, validated by a Council of State
 *   annulment of the 1934 cabinet decision. This constraint story captures
 *   the Islamic sovereignty reading of the site's legitimacy: that sovereign
 *   authority derives from the 1453 Ottoman conquest and continuous waqf
 *   status, making the site an Islamic worship space under Turkish state
 *   control. The reading is contested by Orthodox restitution and universal
 *   heritage sibling readings. The constraint operates as a tangled rope: it
 *   genuinely coordinates worship and sovereignty for an Islamic constituency
 *   while asymmetrically extracting access, jurisdiction, and symbolic
 *   standing from non-Muslim visitors, UNESCO, and secularist Turks. Metrics
 *   and claim are independently authored; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - akp_political_coalition: Agenda-setter (institutional/constrained) â issues decrees, controls courts, captures political-religious legitimacy.
 *   - turkish_islamic_constituency: Beneficiary (organized/identity_locked) â gains sovereign worship space and identity affirmation.
 *   - broader_sunni_ummah: Symbolic beneficiary (organized/identity_locked/global, non-agent) â transnational religious prestige.
 *   - non_muslim_visitors: Payer (moderate/mobile) â bears access restrictions and symbolic subordination.
 *   - unesco_regime: Payer (institutional/constrained) â bears eroded heritage jurisdiction and precedent damage.
 *   - secularist_turks: Payer (organized/identity_locked) â bears ideological defeat of Kemalist secular framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '186b0796-e6ea-46fd-9aea-fbcacd5ed78a').
narrative_ontology:cs_kernel_codification('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', formalized).
narrative_ontology:cs_authority_grounding('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', lineage).
narrative_ontology:cs_interpretation_layer_present('186b0796-e6ea-46fd-9aea-fbcacd5ed78a').
narrative_ontology:cs_reading_relation('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_reading_relation('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', foundational, conquest_establishes_permanent_waqf).
narrative_ontology:cs_axiom_status(conquest_establishes_permanent_waqf, holdable).
narrative_ontology:cs_axiom_grounding('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', conquest_establishes_permanent_waqf, theological).
narrative_ontology:cs_axiom('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', foundational, state_executive_wields_waqf_authority).
narrative_ontology:cs_axiom_status(state_executive_wields_waqf_authority, holdable).
narrative_ontology:cs_axiom_grounding('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', state_executive_wields_waqf_authority, conventional).
narrative_ontology:cs_reference_frame('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', ottoman_waqf_sovereignty).
narrative_ontology:cs_drift_state('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', contemporary_republic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('186b0796-e6ea-46fd-9aea-fbcacd5ed78a', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, broader_sunni_ummah).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, ottoman_waqf_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the executive branch and judicial influence apparatus; issued the 2020 presidential decree converting the museum to a mosque and secured the Council of State annulment of the 1934 decision. Captures concentrated political-religious legitimacy and nationalist consolidation from the act.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, beneficiary).

% Gains a major sovereign worship space in Istanbul; the conversion affirms religious identity and Ottoman continuity narratives. Exit from this identity-framing is costly because the site is a national religious symbol.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, identity_locked, national).

% Symbolically benefits from the reassertion of Islamic sovereignty over an historically significant waqf; reinforces transnational Sunni historical memory. As a diffuse symbolic collective, it does not act directly but is invoked in legitimacy discourse.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, broader_sunni_ummah, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__islamic_sovereignty_reading, broader_sunni_ummah).

% Face restricted access to certain areas, gender-specific entry rules, and a worship environment that supersedes the site's previous museum neutrality; the visit is now structurally subordinate to Islamic prayer function. They bear the cost of diminished heritage access and altered symbolic framing.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    moderate, immediate, mobile, global).

% Claims World Heritage jurisdiction and monitoring authority over the site; its conservation and status-change protocols were bypassed by the unilateral Turkish state action. Bears the cost of eroded institutional authority and a contested precedent for heritage governance.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime, payer,
    institutional, generational, constrained, global).

% Experience the conversion as an ideological defeat of the secular republican framework; the 1934 museum status was a foundational symbol of Kemalist secular modernity. Their identity is constituted partly through that status, making its loss a direct extraction of symbolic capital.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes Islamic worship space for a sovereign state constituency and consolidates shared religious-national identity around a historic waqf site under state administration.
% TRANSFER_FUNCTION: Moves exclusive symbolic and jurisdictional authority over the site from an international heritage-neutral framework and domestic secularist governance to a Turkish state-Islamic sovereignty framework; imposes access restrictions and ideological costs on non-Muslim visitors, UNESCO, and secularist Turks in exchange for political-religious capital accruing to the AKP and Islamic constituencies.
% ABSENT_VOICES: Byzantine and Orthodox ecclesiastical heirs, the Greek state, and ecumenical patriarchate authorities are structurally excluded from the sovereignty conversation; they would assert the primacy of the site's founding Christian purpose but are not seated in Turkish domestic legal or political process.
% DISAPPEARANCE_RATIONALE: If the Islamic sovereignty claim vanished overnight, the site would revert to museum or shared-heritage status, UNESCO jurisdiction would be restored, non-Muslim access would equalize, and the AKP's political symbolism would lose a major anchor; the domestic and international heritage regime would reorganize.
% FOUNDING_PROBLEM: The site's governance after 1934 left it as a secular museum under a Kemalist framework that, from this reading's perspective, suppressed its continuous Islamic waqf identity and denied a sovereign state the right to administer a conquered endowment according to its own religious law.
% FOUNDING_PROBLEM_CORROBORATION: The AKP and Turkish Islamic legal scholars attest the problem from inside the beneficiary set. No independent external corroboration exists; secularist Turks, UNESCO, and Orthodox authorities explicitly deny that a founding problem existed, asserting the 1934 settlement was legitimate and final.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is moderate-high because the conversion imposes real costs on excluded parties (restricted access, lost jurisdiction, ideological damage) while generating concentrated political-religious benefits. Suppression (0.75) is high because the arrangement persists through active state enforcement â court rulings, security control of access, diplomatic rejection of UNESCO intervention â rather than voluntary coordination. Theater ratio (0.45) reflects significant performative sovereignty display (prayer broadcasts, political rallies) layered atop genuine worship function. Accessibility collapse (0.70) is high because the museum-neutrality alternative was legally annulled and is now practically inaccessible; resistance (0.55) is moderate because international and domestic secular opposition is vocal but structurally overridden. The measurement series shows a sharp regime shift at t=20 (2020), when enforcement and extraction spiked.
 *
 * PERSPECTIVAL GAP:
 *   From the AKP seat, the constraint is restoration of legitimate sovereignty and worship coordination; from the UNESCO, secularist, and non-Muslim visitor seats, it is unilateral extraction of heritage neutrality and access rights. The engine computes this divergence from the structural data: agenda_setter with constrained exit vs. payers with constrained or identity-locked exit.
 *
 * DIRECTIONALITY LOGIC:
 *   AKP and the Turkish Islamic constituency sit near the beneficiary end (low d): the constraint subsidizes their political and religious identity projects. The broader Sunni ummah is a diffuse symbolic beneficiary. Non-Muslim visitors are moderate-power targets with mobile exit (they can leave), but their spatial scope is global and their access to the specific heritage experience is trapped. UNESCO is an institutional target with constrained exit (limited enforcement tools against sovereignty). Secularist Turks are identity-locked targets (their self-concept is fused to the 1934 settlement), giving them high d and amplified effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a scaffold because it carries no sunset clause and its justification is steady-state sovereignty, not transition. It is not a piton because the beneficiary (AKP) is concentrated and actively maintains it for political gain. It is not a snare because there is a genuine coordination function (mosque operation for a worship community). It is not a mountain because the historical continuity claim is contested and constructed, not an irreducible natural limit. It is not a rope because the extraction is asymmetric and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_continuity_historicity,
    'Is the claimed continuous Islamic waqf endowment from 1453 historically and legally continuous, or retroactively reconstructed to serve contemporary sovereignty?',
    'Archival and legal-historical analysis of waqf records, court registers, and administrative continuity across the 1934â2020 interval.',
    'If reconstructed, the constraint''s legitimacy is largely performative and classification shifts toward snare or piton; if continuous, the coordination function is stronger and the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_continuity_historicity, empirical, 'Whether the waqf continuity claim is historically grounded or constructed.').

omega_variable(
    kernel_location_ambiguity,
    'This constraint is one reading of the hagia_sophia_substrate kernel. Does the disagreement among readings reduce to empirical facts, or is it located in incommensurable normative frameworks (Islamic waqf law, international heritage law, Orthodox ecclesiastical property)?',
    'No empirical resolution possible; the ambiguity is conceptual and depends on which jurisdictional framework is granted primacy.',
    'If the frameworks are incommensurable, the kernel is irreducibly contested and the extraction measured here is the cost of imposing one framework over others; if commensurable, a hybrid status might lower extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_location_ambiguity, conceptual, 'Sibling reading disagreement location and commensurability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hagi_tr_t10, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(hagi_tr_t14, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(hagi_tr_t20, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(hagi_tr_t22, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 22, 0.5).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hagi_be_t10, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(hagi_be_t14, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 14, 0.3).
narrative_ontology:measurement(hagi_be_t20, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hagi_be_t22, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 22, 0.64).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hagi_su_t10, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(hagi_su_t14, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 14, 0.25).
narrative_ontology:measurement(hagi_su_t20, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hagi_su_t22, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 22, 0.73).
narrative_ontology:measurement(hagi_su_t24, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 24, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hagia_sophia_substrate kernel, which decomposes into three structurally distinct claims: islamic_sovereignty_reading (Ottoman waqf continuity), orthodox_restitution_reading (Byzantine founding legitimacy), and universal_heritage_reading (transcendent shared heritage). The epsilon values and beneficiary/victim structures differ widely across the family; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
