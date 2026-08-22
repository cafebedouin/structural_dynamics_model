% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Brandenburg Imminent-Lawless-Action Standard (Absolutist Reading)
 *   domain: constitutional_law/speech_regulation
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment holds that speech
 *   protection is near-absolute, with the sole exception for direct
 *   incitement to imminent lawless action as articulated in Brandenburg v.
 *   Ohio (1969). This reading maximizes the protected set: hate speech,
 *   harassment, extremist propaganda, disinformation, and offensive
 *   expression all fall within the protection zone unless they meet the
 *   narrow imminence and likelihood thresholds. The unprotected set contains
 *   virtually nothing beyond direct incitement. Minoritized communities bear
 *   the aggregate harm of this protected speech — hate campaigns, stochastic
 *   terrorism radicalization, dignitary harm, and chilling effects on their
 *   own speech — as an externality. The reading presents itself as a natural
 *   constraint on government power (a mountain), but the beneficiary
 *   structure (speakers, press, platforms, advocacy organizations) and the
 *   concentrated externality on minoritized communities create a
 *   false_summit_mountain candidate.
 *
 * KEY AGENTS:
 *   - speakers_of_protected_speech: Primary beneficiary (powerful/mobile) — enjoys maximized protection
 *   - press_institutions: Primary beneficiary (institutional/arbitrage) — institutional speech rights expanded
 *   - advocacy_organizations: Primary beneficiary (organized/mobile) — can advocate extremist positions without suppression
 *   - minoritized_communities: Primary victim (powerless/constrained) — bears concentrated harm externalities
 *   - platforms: Agenda setter (institutional/arbitrage) — governs speech de facto under absolutist-inspired policies
 *   - government: Constrained actor (institutional/analytical) — barred from regulating beyond Brandenburg
 *   - courts: Observer/analytical (analytical/analytical) — adjudicates boundary cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.12).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.08).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Brandenburg Imminent-Lawless-Action Standard (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/speech_regulation").

domain_priors:emerges_naturally(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '7811ee45-b19d-453d-a296-02935127095c').
narrative_ontology:cs_kernel_codification('7811ee45-b19d-453d-a296-02935127095c', formalized).
narrative_ontology:cs_authority_grounding('7811ee45-b19d-453d-a296-02935127095c', lineage).
narrative_ontology:cs_interpretation_layer_present('7811ee45-b19d-453d-a296-02935127095c').
narrative_ontology:cs_reading_relation('7811ee45-b19d-453d-a296-02935127095c', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('7811ee45-b19d-453d-a296-02935127095c', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('7811ee45-b19d-453d-a296-02935127095c', foundational, imminent_lawless_action_only_exception).
narrative_ontology:cs_axiom_status(imminent_lawless_action_only_exception, holdable).
narrative_ontology:cs_axiom_grounding('7811ee45-b19d-453d-a296-02935127095c', imminent_lawless_action_only_exception, deontological).
narrative_ontology:cs_axiom('7811ee45-b19d-453d-a296-02935127095c', foundational, content_neutrality_as_absolute_constraint).
narrative_ontology:cs_axiom_status(content_neutrality_as_absolute_constraint, holdable).
narrative_ontology:cs_axiom_grounding('7811ee45-b19d-453d-a296-02935127095c', content_neutrality_as_absolute_constraint, deontological).
narrative_ontology:cs_axiom('7811ee45-b19d-453d-a296-02935127095c', secondary, more_speech_remedy_sufficiency).
narrative_ontology:cs_axiom_status(more_speech_remedy_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('7811ee45-b19d-453d-a296-02935127095c', more_speech_remedy_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('7811ee45-b19d-453d-a296-02935127095c', brandenburg_1969_doctrinal_settlement).
narrative_ontology:cs_drift_state('7811ee45-b19d-453d-a296-02935127095c', contemporary_digital_amplification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7811ee45-b19d-453d-a296-02935127095c', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers_of_protected_speech).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, press_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, advocacy_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, government).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, brandenburg_standard).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, content_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups whose speech — including hate speech, extremist propaganda, and offensive expression — receives near-absolute protection. They can speak freely without fear of government sanction unless they meet the Brandenburg imminence threshold. Exit is mobile: they can speak in any forum, and the constraint protects their access.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers_of_protected_speech, beneficiary,
    powerful, biographical, mobile, national).

% Media organizations that benefit from maximal editorial freedom and protection from prior restraint, libel law constraints, and content-based regulation. They can publish controversial material with minimal legal risk. Exit is arbitrage-grade: they operate across jurisdictions and platforms.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, press_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Civil liberties groups, ideological advocacy organizations, and legal defense funds that use the absolutist standard to challenge any speech restriction. They gain institutional legitimacy and fundraising capacity from defending the broadest possible protection. Exit is mobile: they can shift issues, venues, and strategies.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Racial, religious, gender, sexual orientation, and other minoritized groups that bear the concentrated harm of protected hate speech, harassment, extremist propaganda, and stochastic terrorism radicalization. They cannot exit the public sphere where this speech operates; counterspeech is often drowned out by coordinated campaigns; legal remedies are foreclosed by the Brandenburg standard. The harm is an externality of the constraint's design.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, constrained, national).

% Social media and communications platforms that govern de facto speech boundaries through content moderation policies often inspired by absolutist rhetoric. They capture the coordination benefits of open discourse (network effects, engagement, ad revenue) while externalizing moderation costs and harm onto users. They are not state actors but functionally govern the speech environment. Exit is arbitrage-grade: they operate globally and can jurisdictional-arbitrage regulation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, platforms, agenda_setter,
    institutional, generational, arbitrage, global).

% Legislative and executive branches barred from regulating speech beyond the narrow Brandenburg exception. They bear the cost of being unable to address harms (disinformation, hate campaigns, foreign interference) that fall within the protected set. Their exit is analytical: they can only change the constraint through constitutional amendment or Court composition shifts.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, government, payer,
    institutional, biographical, analytical, national).

% The judiciary, particularly the Supreme Court, that adjudicates boundary cases and maintains the doctrinal line. They neither collect rents nor bear direct costs but hold the authoritative interpretation. Their exit is analytical: they interpret from within the framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government from suppressing dissent, minority viewpoints, and unpopular speech by establishing a clear, high threshold (imminent lawless action) that is difficult for the state to meet. Solves the coordination problem of credible commitment: the state cannot easily move the line once the standard is set.
% TRANSFER_FUNCTION: Transfers regulatory power from government (which loses the ability to restrict harmful speech) to speakers and platforms (which gain near-absolute freedom). The harm of protected speech is transferred to minoritized communities as an uncompensated externality. No direct monetary transfer occurs, but the cost of speech harms is socialized onto vulnerable groups while the benefits of free expression are captured by speakers and platforms.
% ABSENT_VOICES: Minoritized communities most affected by hate speech and extremist propaganda are structurally excluded from the doctrinal formulation — the Brandenburg test was crafted without their participation, and the Court's 'more speech' remedy assumes a level playing field that does not exist. Victims of stochastic terrorism and radicalization have no seat at the table. Foreign actors exploiting absolutist protections for influence operations are also absent from the domestic constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished overnight, governments could enact hate speech laws, harassment regulations, disinformation controls, and platform accountability measures. The speech environment would reorganize around harm-based or balancing standards. Minoritized communities would gain legal remedies; platforms would face liability for amplification; advocacy organizations would lose their primary litigation tool. The world would rearrange significantly.
% FOUNDING_PROBLEM: In 1969, the Court in Brandenburg confronted a regime where states used sedition and criminal syndicalism laws to punish advocacy of abstract ideas — particularly civil rights, anti-war, and labor organizing. The founding problem was preventing government from criminalizing political dissent by labeling it 'dangerous' or 'subversive' without evidence of imminent action.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Harry Kalven, Geoffrey Stone) document that the sedition law regime Brandenburg targeted has been dismantled. The civil rights movement succeeded; anti-war protest is normalized; labor organizing is legally protected. No serious scholar argues the 1969 threat landscape persists. The ACLU and other civil liberties organizations (beneficiaries) argue the problem is live because 'new threats' always emerge — but this is self-interested testimony. Independent historical analysis confirms the founding problem is dead.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_boundary__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.12) because the constraint formally extracts little — it primarily restricts government action. Suppression is very low (0.08) because the constraint is a limit on state power, not an active coercive regime. Theater ratio is low but rising (0.15) as performative 'free speech' invocations increasingly cover platform power and extremist coordination. Accessibility collapse is high (0.88) — once the Brandenburg standard is understood, alternative regulatory frameworks (harm-based, balancing) appear legally foreclosed within this reading's framework. Resistance is low (0.22) — the reading dominates First Amendment doctrine and faces little effective institutional challenge. The claimed type is mountain (genuine natural constraint on power), but the beneficiary declarations and omegas document the false_summit_mountain candidate: identifiable beneficiaries exist, and the harm externality on minoritized communities may constitute extraction the mountain framing obscures.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker/press/platform seats, the constraint appears as a mountain — a genuine, near-natural limit on government overreach that enables free discourse. From minoritized community seats, the same constraint operates as a snare — it maximizes the speech that harms them while foreclosing regulatory remedies. The engine computes this divergence from the structural data: beneficiaries get low directionality (d near 0), minoritized communities get high directionality (d near 1) despite not being formal targets of state action, because the constraint's structure channels harm to them systematically.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: speakers_of_protected_speech, press_institutions, advocacy_organizations — they gain expanded protection and face minimal cost. Platforms as agenda_setters gain governance authority without accountability. Government is constrained (d ~ 0.5, symmetric costs/benefits of limited power). Minoritized communities are not formal victims of state action but bear concentrated harm externalities — structurally, they are payers with high directionality because the constraint's design maximizes the speech that targets them while minimizing their exit options (constrained exit: they cannot exit the public sphere, and counterspeech is often ineffective against coordinated campaigns). The omega variables capture the ambiguity: is this natural law or constructed extraction?
 *
 * MANDATROPHY ANALYSIS:
 *   The Brandenburg standard was forged in 1969 against a background of sedition laws and civil rights suppression — the founding problem was preventing government from punishing advocacy of abstract ideas. That founding problem is arguably dead (sedition laws are gone, civil rights movement succeeded), but the arrangement persists and has expanded to cover harms (online radicalization, stochastic terrorism, platform amplification) the founders did not anticipate. The constraint may now serve as a mandatrophy: a coordination solution (protecting dissent) that has outlived its founding conditions and now subsidizes harm concentration. The omegas and beneficiary declarations document this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness,
    'Is the Brandenburg standard a genuine natural-law-like constraint on government power, or a constructed doctrinal settlement that benefits identifiable institutional actors?',
    'Historical analysis of the standard''s emergence from Schenck through Brandenburg, tracing whether the ''imminent lawless action'' formulation was a discovered limit or a negotiated compromise between Court factions and political branches.',
    'If constructed, the false_summit_mountain signature may reclassify this constraint as tangled_rope or snare depending on beneficiary/victim structure; if genuine natural law, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness, conceptual, 'Natural-law status vs. constructed doctrinal settlement of the Brandenburg standard.').

omega_variable(
    harm_externality_distribution,
    'Does the absolutist reading''s maximization of the protected set structurally concentrate the harms of protected speech on minoritized communities as an externality, and if so, does that concentration constitute extraction?',
    'Empirical study of hate speech, harassment, and extremist propaganda impacts across demographic groups under Brandenburg-level protection, compared to jurisdictions with broader harm-based restrictions.',
    'If harm concentration is systematic and severe, the constraint may extract from minoritized communities (victims) to subsidize speakers and platforms (beneficiaries), shifting classification toward tangled_rope or snare despite low formal suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_externality_distribution, empirical, 'Whether the externality structure of near-absolute protection constitutes asymmetric extraction.').

omega_variable(
    platform_power_mediation,
    'Does the absolutist reading''s application to private platform governance (via Section 230, common carrier arguments, or state action doctrine) create a structural power asymmetry where platforms become de facto speech governors unaccountable to the communities bearing harm externalities?',
    'Institutional analysis of content moderation decisions, transparency reports, and appeal outcomes for minoritized users vs. majority users under absolutist-inspired platform policies.',
    'If platforms capture the coordination benefits of open discourse while externalizing moderation costs onto vulnerable users, the constraint operates as a tangled_rope with platforms as agenda_setters and minoritized users as payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_power_mediation, empirical, 'Whether private platform power mediates the absolutist reading into an extractive structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_prot_abs_tr_t0, speech_protection_boundary__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(speech_prot_abs_tr_t10, speech_protection_boundary__absolutist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(speech_prot_abs_tr_t20, speech_protection_boundary__absolutist_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(speech_prot_abs_tr_t30, speech_protection_boundary__absolutist_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(speech_prot_abs_tr_t40, speech_protection_boundary__absolutist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(speech_prot_abs_tr_t50, speech_protection_boundary__absolutist_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(speech_prot_abs_be_t0, speech_protection_boundary__absolutist_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(speech_prot_abs_be_t10, speech_protection_boundary__absolutist_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(speech_prot_abs_be_t20, speech_protection_boundary__absolutist_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(speech_prot_abs_be_t30, speech_protection_boundary__absolutist_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(speech_prot_abs_be_t40, speech_protection_boundary__absolutist_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(speech_prot_abs_be_t50, speech_protection_boundary__absolutist_reading, base_extractiveness, 50, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(speech_prot_abs_su_t0, speech_protection_boundary__absolutist_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(speech_prot_abs_su_t10, speech_protection_boundary__absolutist_reading, suppression_requirement, 10, 0.04).
narrative_ontology:measurement(speech_prot_abs_su_t20, speech_protection_boundary__absolutist_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(speech_prot_abs_su_t30, speech_protection_boundary__absolutist_reading, suppression_requirement, 30, 0.06).
narrative_ontology:measurement(speech_prot_abs_su_t40, speech_protection_boundary__absolutist_reading, suppression_requirement, 40, 0.07).
narrative_ontology:measurement(speech_prot_abs_su_t50, speech_protection_boundary__absolutist_reading, suppression_requirement, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.02).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, platform_content_moderation_authority).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, hate_speech_regulation_boundary).

% DUAL FORMULATION NOTE:
% This constraint family (speech_protection_boundary) decomposes the single colloquial label 'First Amendment speech protection' into three structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. The absolutist_reading claims mountain with low ε but declares beneficiaries (false_summit_mountain candidate). The harm_limited_reading and balancing_reading will likely classify as tangled_rope or snare with higher ε and explicit victim sets. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
