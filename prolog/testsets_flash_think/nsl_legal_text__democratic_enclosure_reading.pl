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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law (Democratic Enclosure Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the Hong Kong National Security Law (NSL)
 *   from the perspective of 'democratic enclosure.' It views the NSL not
 *   primarily as a genuine security measure, but as a mechanism designed to
 *   permanently close democratic space, criminalize dissent, and dismantle
 *   the 'one country, two systems' framework's promise of autonomy. The law's
 *   broad definitions of offenses and extraterritorial reach are interpreted
 *   as tools for political control, leading to high extraction of civil
 *   liberties and severe suppression of opposition.
 *
 * KEY AGENTS:
 *   - beijing_government: Primary agenda_setter (institutional/arbitrage) — benefits from control
 *   - hong_kong_establishment: Beneficiary (powerful/constrained) — benefits from stability and reduced opposition
 *   - hong_kong_civil_society: Primary payer (powerless/trapped) — bears the costs of lost freedoms
 *   - pro_democracy_activists: Primary payer (powerless/identity_locked) — bears the costs of criminalization and repression
 *   - independent_media: Payer (moderate/constrained) — bears the costs of censorship and closure
 *   - international_human_rights_advocates: Observer (organized/analytical) — documents abuses, advocates for change
 *   - international_businesses: Beneficiary (powerful/mobile) — benefits from 'stability', can exit if costs outweigh benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.95).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law (Democratic Enclosure Reading)").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '68d953d0-a966-4f70-a796-49d8d914da14').
narrative_ontology:cs_kernel_codification('68d953d0-a966-4f70-a796-49d8d914da14', formalized).
narrative_ontology:cs_authority_grounding('68d953d0-a966-4f70-a796-49d8d914da14', extraction).
narrative_ontology:cs_interpretation_layer_present('68d953d0-a966-4f70-a796-49d8d914da14').
narrative_ontology:cs_reading_relation('68d953d0-a966-4f70-a796-49d8d914da14', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('68d953d0-a966-4f70-a796-49d8d914da14', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('68d953d0-a966-4f70-a796-49d8d914da14', foundational, political_pluralism_is_essential).
narrative_ontology:cs_axiom_status(political_pluralism_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('68d953d0-a966-4f70-a796-49d8d914da14', political_pluralism_is_essential, deontological).
narrative_ontology:cs_axiom('68d953d0-a966-4f70-a796-49d8d914da14', foundational, state_power_must_be_constrained).
narrative_ontology:cs_axiom_status(state_power_must_be_constrained, holdable).
narrative_ontology:cs_axiom_grounding('68d953d0-a966-4f70-a796-49d8d914da14', state_power_must_be_constrained, deontological).
narrative_ontology:cs_reference_frame('68d953d0-a966-4f70-a796-49d8d914da14', one_country_two_systems_autonomy).
narrative_ontology:cs_drift_state('68d953d0-a966-4f70-a796-49d8d914da14', post_nsl_enactment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('68d953d0-a966-4f70-a796-49d8d914da14', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_media).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, international_human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, international_businesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate authority that enacted the NSL, using it to assert control over Hong Kong's political and social landscape. Benefits from the suppression of dissent and the consolidation of power, viewing it as essential for national security and stability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Local political and business elites who align with Beijing, benefiting from the stability and reduced political opposition the NSL provides. They administer the law and its enforcement, gaining influence and economic opportunities within the new political order.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, beneficiary,
    powerful, generational, constrained, national).

% A broad range of non-governmental organizations, unions, and community groups whose activities are now severely restricted or criminalized. They bear the costs of reduced freedoms, self-censorship, and the dismantling of democratic institutions.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society, payer,
    powerless, biographical, trapped, local).

% Individuals who advocate for democratic reforms, now facing arrest, imprisonment, or exile under the NSL. Their identity is deeply tied to their activism, making exit from the struggle difficult despite severe personal risks.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_activists, payer,
    powerless, immediate, identity_locked, local).

% News outlets and journalists that previously provided critical reporting, now operating under immense pressure, self-censorship, or forced closure. They bear the cost of diminished press freedom and the inability to hold power accountable.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_media, payer,
    moderate, biographical, constrained, local).

% Organizations and individuals globally who monitor human rights in Hong Kong, documenting abuses and advocating for international intervention. They observe the constraint's impact and provide external pressure, but have limited direct influence on its operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% Corporations operating in Hong Kong that prioritize 'stability' and a predictable (if authoritarian) business environment. They benefit from the suppression of protests and political uncertainty, even if it comes at the cost of democratic freedoms, and can relocate if conditions become too unfavorable.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_businesses, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_government).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate national security and social stability by criminalizing acts of secession, subversion, terrorism, and collusion with foreign forces, thereby ensuring the 'one country, two systems' framework operates without perceived threats to sovereignty.
% TRANSFER_FUNCTION: Transfers democratic freedoms, political agency, and civil liberties from Hong Kong citizens, civil society, and independent media to the Beijing and Hong Kong establishment, consolidating political control and suppressing dissent.
% ABSENT_VOICES: Pro-democracy political parties (disbanded or exiled), independent judiciary (eroded by NSL interpretations), international legal bodies (whose rulings are ignored), and a significant portion of the Hong Kong populace who fear speaking out.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement vanished overnight, there would be an immediate resurgence of democratic activity, re-establishment of independent media, and a renewed push for greater autonomy and human rights. The political landscape of Hong Kong would fundamentally reorganize.
% FOUNDING_PROBLEM: The alleged threats of secession, subversion, terrorism, and collusion with foreign forces, particularly in the wake of the 2019 anti-government protests, which Beijing framed as a direct challenge to national sovereignty and stability.
% FOUNDING_PROBLEM_CORROBORATION: The Beijing and Hong Kong establishment attest that the founding problem is still live, citing ongoing security threats and foreign interference. International bodies, human rights groups, and exiled activists attest that the founding problem was largely a pretext for political control, and that the NSL has exacerbated, rather than solved, underlying grievances. Legislative hearing testimony and independent analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very high (0.92) because the NSL fundamentally redefines the relationship between the state and its citizens, stripping away previously guaranteed freedoms and political participation. Suppression is extremely high (0.95) due to the severe penalties, broad scope of offenses, and the chilling effect on speech and assembly. The enforcement machinery actively targets and neutralizes any form of dissent. Theater ratio is moderate (0.40) because while there are genuine security concerns that the law purports to address, a significant portion of its application and enforcement is directed at political control rather than preventing actual acts of terrorism or subversion as understood in liberal democracies. Accessibility collapse is high (0.90) as nearly all avenues for democratic expression and opposition have been shut down or criminalized. Resistance remains moderate (0.70) due to ongoing international condemnation and the persistent, albeit suppressed, efforts of activists and civil society members.
 *
 * PERSPECTIVAL GAP:
 *   The Beijing government and Hong Kong establishment perceive the NSL as a necessary and legitimate tool for restoring order and national security, thus experiencing it as a 'rope' or even a 'mountain' of sovereign authority. In contrast, Hong Kong civil society, pro-democracy activists, and independent media experience it as a 'snare' – a highly extractive and suppressive mechanism designed to eliminate their political space. International human rights advocates largely align with the 'snare' perspective, while some international businesses may view it as a 'tangled rope' that provides stability at a cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The Beijing government and Hong Kong establishment are clear beneficiaries, gaining consolidated power and reduced opposition (low d). Hong Kong civil society, pro-democracy activists, and independent media are direct targets, losing freedoms and facing criminalization (high d). International businesses are indirect beneficiaries, valuing 'stability' (low d). International human rights advocates are observers, neither directly benefiting nor paying, but critically analyzing the constraint's impact (analytical d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the NSL as a legitimate 'rope' for national security, as claimed by its proponents. By highlighting the high extractiveness and suppression, and identifying clear victims, it exposes the underlying function as one of political enclosure and control, rather than genuine coordination for collective benefit. The 'contested' status of the founding problem further supports this, indicating that the original justification is now a cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_legitimacy_ambiguity,
    'Is the NSL a legitimate national security instrument addressing genuine threats, or primarily a tool for political repression and democratic enclosure?',
    'Independent international legal review of NSL prosecutions, comparative analysis with international human rights law, and empirical assessment of actual security threats versus political dissent targeted.',
    'If primarily repression, the constraint''s extractiveness and suppression are confirmed as illegitimate, strengthening its ''snare'' classification. If genuine security threats are found to be the primary target, the ''theater_ratio'' might decrease, and the ''claimed_type'' could shift towards a ''tangled_rope'' with a more defensible coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nsl_legitimacy_ambiguity, conceptual, 'Ambiguity between security justification and political repression.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, police action) or internalized (self-censorship, fear) within Hong Kong civil society?',
    'Post-NSL survey data on self-censorship and changes in public discourse, combined with analysis of legal enforcement patterns. If suppression persists significantly even after direct legal threats are removed (e.g., for exiled communities), it indicates a strong internalized component.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measures alone suggest, as the targets carry the suppression with them, making exit from the ''enclosed'' mindset more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    founding_problem_pretext,
    'To what extent was the ''founding problem'' (secession, subversion, etc.) a genuine threat requiring the NSL, versus a pretext to justify political control?',
    'Declassified intelligence reports, independent historical analysis of the 2019 protests, and expert testimony from international security analysts and legal scholars outside of the directly benefiting parties.',
    'If largely a pretext, the ''founding_problem_status'' shifts definitively to ''dead'', reinforcing the ''snare'' classification and highlighting the mandatrophy of the claimed coordination function. If genuine threats are substantiated, it would lend more credence to the ''sovereignty_restoration_reading'' and potentially lower the ''extractiveness'' from this reading''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_pretext, empirical, 'Whether the NSL''s justification was a genuine problem or a pretext.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2024, 0.39).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2021, 0.85).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2022, 0.88).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2023, 0.9).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2024, 0.91).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2025, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2021, 0.9).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2022, 0.92).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2023, 0.93).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2024, 0.94).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2025, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_electoral_system_reform).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_press_freedom).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_judicial_independence).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nsl_legal_text' kernel, focusing on its role in democratic enclosure. It is structurally distinct from the 'sovereignty_restoration_reading' and 'jurisdictional_capture_reading', which analyze different aspects of the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
