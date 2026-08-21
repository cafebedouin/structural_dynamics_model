% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: National Security Law (Hong Kong): Democratic Enclosure Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'democratic enclosure' reading of
 *   the Hong Kong National Security Law (NSL). From this perspective, the NSL
 *   is a snare designed to permanently close democratic space, criminalize
 *   dissent, and dismantle civil society in Hong Kong. It is not primarily
 *   about genuine national security threats, but about consolidating
 *   political control. The high extractiveness and suppression reflect the
 *   systematic dismantling of democratic institutions and freedoms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.95).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "National Security Law (Hong Kong): Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, 'ab184ad1-8c6c-4b77-8c6d-5bb93aad9834').
narrative_ontology:cs_kernel_codification('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', fixed_text).
narrative_ontology:cs_authority_grounding('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', extraction).
narrative_ontology:cs_interpretation_layer_present('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834').
narrative_ontology:cs_reading_relation('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', foundational, democratic_rights_are_inalienable).
narrative_ontology:cs_axiom_status(democratic_rights_are_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', democratic_rights_are_inalienable, deontological).
narrative_ontology:cs_axiom('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', foundational, national_security_as_political_pretext).
narrative_ontology:cs_axiom_status(national_security_as_political_pretext, holdable).
narrative_ontology:cs_axiom_grounding('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', national_security_as_political_pretext, empirically_contingent).
narrative_ontology:cs_reference_frame('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', one_country_two_systems_autonomy).
narrative_ontology:cs_drift_state('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', post_nsl_enactment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ab184ad1-8c6c-4b77-8c6d-5bb93aad9834', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_media).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary architect and enforcer of the NSL, viewing it as a necessary tool to restore stability and assert sovereignty. Benefits from the suppression of dissent and the consolidation of political control over Hong Kong.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Local political and business elites who align with Beijing. They benefit from the removal of political opposition and the perceived return to stability, which facilitates their economic and political interests, albeit under increased central control.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, beneficiary,
    institutional, biographical, constrained, national).

% Organizations and activists advocating for democratic reforms, human rights, and autonomy. They bear the direct costs of the NSL through arrests, forced closures, and self-censorship, leading to the dismantling of their networks.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society, payer,
    powerless, immediate, trapped, local).

% Journalists and news outlets that provided critical coverage of the government. They face legal threats, asset freezes, and pressure to self-censor, leading to the closure of prominent independent news organizations and a chilling effect on reporting.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_media, payer,
    moderate, immediate, identity_locked, local).

% Elected representatives and political figures who advocated for greater democracy. They have been disqualified, arrested, and imprisoned under the NSL, effectively eliminating organized political opposition within the legislative framework.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_politicians, payer,
    powerless, biographical, trapped, local).

% The general populace of Hong Kong, many of whom participated in protests or supported democratic ideals. They experience a pervasive climate of fear, self-censorship, and the erosion of civil liberties, with limited avenues for expressing dissent.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_citizens, payer,
    organized, biographical, constrained, local).

% Monitor and report on human rights abuses in Hong Kong, condemning the NSL's impact on civil liberties. They can exert diplomatic pressure but have no direct enforcement power over the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of this reading, the NSL coordinates the suppression of all forms of political dissent and opposition, ensuring a unified political narrative and control over Hong Kong's governance.
% TRANSFER_FUNCTION: Transfers political power, autonomy, and civil liberties from Hong Kong's democratic institutions, civil society, and citizens to the Beijing central government and its aligned Hong Kong establishment.
% ABSENT_VOICES: Any independent legal scholars, international human rights bodies, or former pro-democracy legislators who would argue for the NSL's illegitimacy under international law or the Basic Law are systematically silenced or excluded from official discourse.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement vanished overnight, Hong Kong's civil society, independent media, and political opposition would rapidly re-emerge, challenging the current political order and demanding a return to greater autonomy and democratic freedoms. The political landscape would be fundamentally reshaped.
% FOUNDING_PROBLEM: The NSL was enacted to address perceived threats to national security, including secession, subversion, terrorism, and collusion with foreign forces, particularly in response to the 2019 anti-government protests.
% FOUNDING_PROBLEM_CORROBORATION: The Beijing central government and the Hong Kong establishment assert that the founding problems are live and severe. However, civil society groups, independent legal experts, and international observers argue that the 'threats' were exaggerated or used as a pretext, and that the NSL's true function is to eliminate political opposition, indicating the problem is largely 'dead' as a genuine security concern and 'live' as a political control mechanism.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the NSL targets fundamental civil liberties, political participation, and the rule of law, transferring these directly to central government control. Suppression is extremely high (0.95) due to the broad scope of the law, its retroactive application, and the severe penalties, which create a pervasive chilling effect and actively eliminate opposition. Theater ratio is low (0.15) because while 'national security' is invoked, the actual enforcement is directly aimed at political control, with little performative pretense masking other functions. Accessibility collapse is high (0.88) as legal and political avenues for dissent have been systematically closed. Resistance remains high (0.75) despite severe suppression, indicating ongoing, albeit often covert, opposition.
 *
 * PERSPECTIVAL GAP:
 *   This reading sharply diverges from the 'sovereignty restoration' reading, which would frame the NSL as a legitimate and necessary measure for national security, with low extractiveness and high coordination. The engine's classification will highlight this divergence based on the structural data provided here.
 *
 * DIRECTIONALITY LOGIC:
 *   The Beijing central government and the Hong Kong establishment are clear beneficiaries, gaining political control and stability. Hong Kong civil society, independent media, pro-democracy politicians, and citizens are the primary victims, losing fundamental rights and freedoms. Their exit options are severely constrained or trapped, leading to high directionality towards being targets of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by asserting that the NSL's original mandate (genuine national security) has either atrophied or was a pretext. The persistence of the constraint is due to active coercion and the benefits it provides to the agenda-setters, not due to a live coordination problem or institutional inertia. It is a snare, not a piton or a rope, because there are clear beneficiaries of the extraction and identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_true_purpose_ambiguity,
    'Is the primary purpose of the NSL genuine national security, or the permanent closure of democratic space and criminalization of dissent?',
    'Analysis of enforcement patterns over time: if enforcement disproportionately targets political speech and assembly rather than acts of violence or foreign espionage, it supports the democratic enclosure reading.',
    'If primarily democratic enclosure, the constraint is a snare with very high extraction. If primarily genuine national security, it would be a tangled rope or even a rope, with lower extraction and a clearer coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsl_true_purpose_ambiguity, conceptual, 'Ambiguity regarding the NSL''s true functional mandate.').

omega_variable(
    international_law_legitimacy,
    'Is the NSL consistent with international human rights law and the Basic Law''s guarantees of autonomy?',
    'Adjudication by international legal bodies (if jurisdiction were accepted) or a consensus opinion from independent international legal scholars.',
    'If inconsistent, the NSL''s legitimacy is undermined, increasing its effective suppression and extractiveness from the perspective of international norms. If consistent, its perceived legitimacy would rise, potentially lowering resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_legitimacy, conceptual, 'Legal legitimacy of the NSL under international and constitutional frameworks.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, arrests) or internalized (self-censorship, fear)?',
    'Post-NSL enforcement trajectory: if suppression persists after initial arrests and legal actions, with widespread self-censorship, it indicates significant internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — citizens carry the suppression with them after exit or even without direct enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Hong Kong.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 3, 0.16).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 1, 0.88).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2, 0.9).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 3, 0.91).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 4, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 1, 0.9).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2, 0.92).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 3, 0.94).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 4, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
