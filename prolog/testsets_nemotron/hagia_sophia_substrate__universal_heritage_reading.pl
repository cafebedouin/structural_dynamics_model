% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Heritage Museum
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The 1935 conversion of Hagia Sophia from imperial mosque to secular
 *   museum instantiated the 'universal heritage' reading: the site's
 *   legitimacy was re-grounded from Islamic endowment (waqf) and Byzantine
 *   cathedral to a transcultural human patrimony administered by the Turkish
 *   Republic's technocratic bureaucracy. This reading served as the
 *   ideological flagship of Kemalist secular modernity — proof that the new
 *   state could transcend religious particularism. The constraint persists
 *   through active enforcement: the 2020 reversion to mosque status by
 *   presidential decree demonstrated that the museum framing was never a
 *   natural equilibrium but a state-maintained regime. The high
 *   extractiveness (ε=0.72) reflects tourism revenue capture and the
 *   ideological rent of secular legitimacy; suppression (0.78) reflects the
 *   legal and physical barring of Islamic worship for 85 years and the
 *   ongoing marginalization of waqf trusteeship. The theater ratio (0.42)
 *   captures the growing gap between the 'shared heritage' performance and
 *   the reality of a site whose conservation priorities increasingly serve
 *   tourist throughput over material integrity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.78).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, snare).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Heritage Museum").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '668773df-a8a9-4068-9cef-f03a59887847').
narrative_ontology:cs_kernel_codification('668773df-a8a9-4068-9cef-f03a59887847', formalized).
narrative_ontology:cs_authority_grounding('668773df-a8a9-4068-9cef-f03a59887847', extraction).
narrative_ontology:cs_interpretation_layer_present('668773df-a8a9-4068-9cef-f03a59887847').
narrative_ontology:cs_reading_relation('668773df-a8a9-4068-9cef-f03a59887847', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('668773df-a8a9-4068-9cef-f03a59887847', hagia_sophia_substrate__orthodox_restitution_reading, influences).
narrative_ontology:cs_axiom('668773df-a8a9-4068-9cef-f03a59887847', foundational, cultural_heritage_transcends_religious_ownership).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_ownership, holdable).
narrative_ontology:cs_axiom_grounding('668773df-a8a9-4068-9cef-f03a59887847', cultural_heritage_transcends_religious_ownership, deontological).
narrative_ontology:cs_axiom('668773df-a8a9-4068-9cef-f03a59887847', foundational, secular_state_has_sole_authority_over_heritage_designation).
narrative_ontology:cs_axiom_status(secular_state_has_sole_authority_over_heritage_designation, holdable).
narrative_ontology:cs_axiom_grounding('668773df-a8a9-4068-9cef-f03a59887847', secular_state_has_sole_authority_over_heritage_designation, conventional).
narrative_ontology:cs_reference_frame('668773df-a8a9-4068-9cef-f03a59887847', kemalist_secular_modernity_founding).
narrative_ontology:cs_drift_state('668773df-a8a9-4068-9cef-f03a59887847', post_2020_reversion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('668773df-a8a9-4068-9cef-f03a59887847', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, technocratic_museum_administration).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, secular_constitutional_supremacy).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, universal_cultural_heritage_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the site as a museum under the Ministry of Culture. Controls access, curation, and revenue streams. Collects ticket revenue and UNESCO-linked funding. Justifies exclusivity of museum framing through secular constitutional law and international heritage conventions. Has structural capacity to maintain or change the regime.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, technocratic_museum_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, technocratic_museum_administration, beneficiary).

% International tourism operators, academic researchers, UNESCO bureaucrats, and heritage NGOs. Benefit from uninterrupted visitor access, research permits, and the site's brand value as a 'meeting of civilizations' symbol. Would lose institutional access and revenue if the site reverted to exclusive worship use. Exit is easy — they redirect tourists and grants to other sites.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector, beneficiary,
    organized, biographical, mobile, global).

% Kemalist military-judicial-bureaucratic networks, CHP political base, secular intelligentsia. The museum status is an ideological flagship: proof that the Republic subordinated religious claims to civilizational universalism. Losing it would be a symbolic defeat for the secular founding myth. Exit is constrained — their identity and political capital are fused to the regime's cultural symbols.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    institutional, generational, constrained, national).

% Conservative Muslim citizens, AKP voter base, religious endowment (waqf) trustees, Imam networks. The site is experienced as a mosque seized by the secular state; the museum framing denies their worship rights and the waqf's legal continuity. They pay through exclusion from their sacred space and the ideological insult of seeing the mihrab covered or treated as an exhibit. Exit is identity-locked: abandoning the claim would fracture communal self-understanding and theological continuity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claimants, payer,
    organized, biographical, identity_locked, national).

% Ecumenical Patriarchate, Greek state, Orthodox diaspora. Claim the site by Byzantine origin and demand either restitution or neutral status honoring its Christian past. Are excluded from both worship and meaningful consultation. The universal heritage frame subsumes their specific claim into a generic 'shared heritage' that erases Byzantine particularity. Exit is constrained — they maintain the claim across generations but lack leverage in Turkish domestic politics.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_restitution_claimants, excluded,
    moderate, generational, constrained, continental).

% UNESCO World Heritage Committee, ICOMOS, academic heritage critics. Monitor compliance with the 1972 Convention and the site's Outstanding Universal Value (OUV) justification. Their assessments legitimize the museum framing internationally but have no enforcement power over Turkish sovereignty.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_heritage_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, internationally recognized framework for preserving and presenting the physical fabric of Hagia Sophia to global audiences, avoiding contested religious administration.
% TRANSFER_FUNCTION: Moves tourism revenue (estimated $60M+ annually), UNESCO-linked conservation funding, and secular nationalist symbolic capital from the site's religious claimants to the technocratic museum administration and the global heritage-tourism complex.
% ABSENT_VOICES: The waqf trustees who hold the Ottoman endowment deed, the congregants who would pray there daily, and the Orthodox faithful who see their Byzantine heritage erased — all are structurally excluded from the governance conversation. Their objection would be theological and legal continuity, not preservation.
% DISAPPEARANCE_RATIONALE: If the museum constraint vanished overnight, the site would revert to active mosque use (as occurred in 2020), tourist flows would be restricted to non-prayer times, revenue would drop sharply, the secularist symbolic flagship would fall, and UNESCO would likely delist or sanction the site. The physical fabric would remain but the governance regime, revenue model, and ideological signal would fundamentally reorganize.
% FOUNDING_PROBLEM: After the Ottoman collapse, the new Turkish Republic needed to signal a break from theocratic empire and membership in the 'civilized nations' club. Converting Hagia Sophia from mosque to museum (1935) solved the legitimization problem: it demonstrated secular modernity to the West while neutralizing the site as a focus of Islamic political mobilization.
% FOUNDING_PROBLEM_CORROBORATION: Secularist historians and CHP archives attest the founding problem was secular legitimation. Islamic and conservative Turkish scholars (e.g., İhsanoğlu, Yavuz) attest the problem was always a pretext for suppressing Islamic sovereignty, noting the waqf's legal continuity was never extinguished. International legal scholars (e.g., O'Keefe, Prott) note the 1935 conversion violated the waqf's inalienable status under Ottoman and early Republican law — corroboration from outside the secularist beneficiary set.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint captures tourism revenue (global sector) and secular ideological capital (domestic elites) while denying the waqf's legal property rights and the claimants' worship access. Suppression is high because the constraint's persistence from 1935–2020 depended on state enforcement — banning prayer, covering mosaics, policing the space — not on stakeholder consent. The museum framing is not a coordination equilibrium: the 'shared heritage' beneficiaries (tourists, scholars) have arbitrage-grade exit, while the payers (Islamic claimants) are identity-locked. The engine will compute per-seat types from this structural asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the museum administration's seat, the constraint is genuine coordination: it preserves the fabric, manages visitors, and satisfies UNESCO. From the Islamic claimant's seat, the same structure is a snare: the 'heritage' language is cover for waqf expropriation and worship denial. From the secularist elite's seat, it is a rope: a coordination mechanism that produces their identity-rent. The engine computes this divergence — the authored claim (snare) reflects the structural asymmetry visible from the payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The technocratic museum administration sits at the beneficiary end (d ≈ 0.15): it controls the asset, collects the revenue, and sets the rules. Secularist elites are near-beneficiary (d ≈ 0.25) — they collect ideological rent but bear political risk when the constraint is challenged. Global tourism/scholarship is symmetric-to-beneficiary (d ≈ 0.35): they benefit from access but can redirect. Islamic worship claimants are full targets (d ≈ 0.95): identity-locked, no exit, bear the full exclusion cost. Orthodox claimants are excluded targets (d ≈ 0.85): constrained exit, structurally silenced. The derivation from beneficiary/victim declarations + exit options produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (secular legitimation of the Republic) was live in 1935. By 2024 the Republic is consolidated, the secularist project is politically contested, and the waqf's legal continuity has been formally acknowledged by the 2020 reversion. Yet the museum constraint persisted for 85 years after the founding problem's peak relevance — a classic mandatrophy signature. The constraint's persistence was not inertia but active maintenance: the museum framing became an end in itself for the beneficiaries (tourism revenue, secularist identity). The 2020 reversion did not erase the constraint's extractive history; it inverted the extraction direction. The snare classification captures the 1935–2020 regime; the current regime is a different constraint (islamic_sovereignty_reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_legal_continuity,
    'Does the Ottoman waqf (endowment) deed retain binding legal force under international law and Turkish domestic law, such that the 1935 museum conversion was an unlawful expropriation?',
    'Comparative analysis of Ottoman land law, the 1924 Turkish Constitution''s property protections, the 1935 Cabinet Decree, and the 2020 Presidential Decree — tested in Turkish Constitutional Court and potentially ECHR.',
    'If the waqf retains legal personality, the museum constraint was a snare from inception (unlawful taking). If the waqf was extinguished by sovereign act, the museum constraint''s extraction is ''only'' ideological and economic, not legal-theft.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_legal_continuity, empirical, 'Whether the constraint''s extraction includes unlawful expropriation of a standing religious endowment.').

omega_variable(
    secularism_as_extraction_cover,
    'Is the ''universal heritage'' framing a genuine coordination solution for preservation, or a cover story invented to legitimize the secularist elite''s ideological capture of the site?',
    'Counterfactual: if preservation were the sole goal, would a shared-governance model (waqf + UNESCO + Patriarchate) have worked? Historical analysis of the 1935 decision records — was waqf consultation sought?',
    'If cover story, the constraint is a pure snare with ε reflecting ideological rent. If genuine coordination, part of the measured ε is the price of a real preservation function (Tangled Rope candidate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularism_as_extraction_cover, conceptual, 'Whether the coordination function is authentic or fabricated.').

omega_variable(
    kernal_reading_framing_ambiguity,
    'Does the ''universal heritage'' reading foreclose the Islamic sovereignty reading, or do they coexist as competing legitimacies within Turkish constitutional politics?',
    'Track whether Turkish courts, the AKP government, and the Directorate of Religious Affairs treat the museum period as a legal nullity (foreclosure) or as a historical episode superseded by sovereign restoration (coexistence).',
    'If foreclosure, the kernel has no stable structure — each reading destroys the other''s legitimacy. If coexistence, the kernel is a persistent site of contestation where readings cycle with political power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernal_reading_framing_ambiguity, conceptual, 'Structural relationship between this reading and the islamic_sovereignty_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1935, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1935, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1935, 0.25).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(hagi_tr_t1985, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1935, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1935, 0.55).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(hagi_be_t1985, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1935, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1935, 0.7).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(hagi_su_t1985, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form a kernel family (hagia_sophia_substrate) where each reading instantiates a different constraint with distinct ε, beneficiary/victim sets, and authority groundings. The universal_heritage_reading (this file) has high ε (0.72) from tourism revenue and secular ideological extraction; the islamic_sovereignty_reading has lower ε (worship access restored, but tourist revenue lost); the orthodox_restitution_reading has contested ε (restitution would transfer control to a non-state actor). All three share the same physical substrate but authorize different extraction-suppression regimes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__universal_heritage_reading, institutional, 0.15).
constraint_indexing:directionality_override(hagia_sophia_substrate__universal_heritage_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
