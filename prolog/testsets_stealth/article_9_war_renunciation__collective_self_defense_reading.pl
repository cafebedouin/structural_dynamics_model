% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Reading (Survival-Triggered Allied Defense Authorization)
 *   domain: constitutional law / security policy / institutional legitimacy
 *
 * SUMMARY:
 *   Since the July 2014 Cabinet decision on the limited exercise of
 *   collective self-defense, Article 9 as administered by the Japanese
 *   executive permits the use of force to defend allied forces when Japan's
 *   survival is threatened and no other appropriate means preserves the
 *   population's rights: the three-condition test. The 2015
 *   peace-and-security statutes built the operational machinery (protection
 *   of US vessels, logistics in combat zones, expanded SDF mission
 *   envelopes), and the 2022 strategic documents extended the posture with
 *   counterstrike acquisition and a defense buildout toward two percent of
 *   GDP. The arrangement genuinely coordinates: it gives commanders, allied
 *   planners, and Diet overseers a single authorization standard where
 *   case-by-case improvisation previously governed. It also transfers: each
 *   elastic reading moves war-authority discretion from the amendment process
 *   to the cabinet, and the payers — the constitutional law professoriate,
 *   antiwar movements, minority legislators, and host communities — bear a
 *   stability loss and risk allocation they did not consent to. This file
 *   instantiates the collective_self_defense_reading of the
 *   article_9_war_renunciation kernel; the strict-pacifist and inherent-right
 *   readings are separate constraint files linked in
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: the claimed type states the structure I believe true; the
 *   metrics state the operation I believe observable.
 *
 * KEY AGENTS:
 *   - - japanese_cabinet: Agenda-setter and primary collector (institutional/constrained) — authors the trigger and receives the discretion
 *   - - united_states_alliance_planners: Primary external beneficiary (powerful/mobile)
 *   - - sdf_officer_corps: Institutional beneficiary and risk bearer (organized/constrained)
 *   - - defense_procurement_sector: Commercial beneficiary (powerful/arbitrage)
 *   - - constitutional_law_professoriate: Doctrinal payer (moderate/identity_locked)
 *   - - antiwar_citizen_movements: Street-level payer (organized/trapped)
 *   - - diet_minority_parties: Procedural payer (organized/constrained)
 *   - - host_community_residents: Excluded burden bearer (powerless/trapped)
 *   - - supreme_court_of_japan: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.64).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.57).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading (Survival-Triggered Allied Defense Authorization)").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional law / security policy / institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223').
narrative_ontology:cs_kernel_codification('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', fixed_text).
narrative_ontology:cs_authority_grounding('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', extraction).
narrative_ontology:cs_interpretation_layer_present('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223').
narrative_ontology:cs_reading_relation('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', foundational, collective_self_defense_survival_trigger_permissible).
narrative_ontology:cs_axiom_status(collective_self_defense_survival_trigger_permissible, holdable).
narrative_ontology:cs_axiom_grounding('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', collective_self_defense_survival_trigger_permissible, deontological).
narrative_ontology:cs_axiom('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', secondary, renunciation_clause_preserves_inherent_sovereign_rights).
narrative_ontology:cs_axiom_status(renunciation_clause_preserves_inherent_sovereign_rights, holdable).
narrative_ontology:cs_axiom_grounding('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', renunciation_clause_preserves_inherent_sovereign_rights, conventional).
narrative_ontology:cs_reference_frame('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', inherent_right_collective_defense_baseline).
narrative_ontology:cs_drift_state('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', post_2022_strategic_document_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('6ce06c5a-8f1c-4fdd-8c5c-7cdeacfce223', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_planners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, sdf_officer_corps).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_procurement_sector).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_law_professoriate).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, antiwar_citizen_movements).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, diet_minority_parties).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, inherent_right_of_self_defense_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, executive_interpretation_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the 2014 cabinet decision and shepherded the 2015 implementing statutes; defines what counts as a survival-threatening situation and decides when the authorization applies. Gains war-authority discretion without a referendum. Having committed to allies on the strength of the new reading, walking back is now politically expensive, so the cabinet defends the line it drew.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet, beneficiary).

% Plan joint operations assuming Japanese protection of US vessels and assets and Japanese logistics support in contingencies. The 2015 statutes removed barriers they had worked around for decades. They can adjust alliance posture or hedge with other partners if Japan reverts, so their reliance is real but not captive.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, united_states_alliance_planners, beneficiary,
    powerful, biographical, mobile, global).

% Gain expanded mission sets, budgets, and roles, from minesweeping in distant waters to guarding allied vessels. The same expansion assigns them to harm's way in contingencies they did not define; career structures bind them to the institution either way.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, sdf_officer_corps, beneficiary,
    organized, biographical, constrained, regional).

% Receives growing orders under the buildup the authorization supports: standoff missiles, vessels, ammunition stockpiles. Contracts diversify across buyers, so revenue does not depend on any single customer's charter.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_procurement_sector, beneficiary,
    powerful, immediate, arbitrage, global).

% Declared the 2014 reinterpretation unconstitutional in large numbers; their authority rests on the text meaning what it says and changing only through amendment. Each elastic reading lowers the price of the next, and their professional standing is tied to a stability the practice steadily erodes. Leaving the field would mean abandoning the discipline's premise.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_law_professoriate, payer,
    moderate, generational, identity_locked, national).

% Mobilized the largest street protests in decades against the 2015 statutes and continue organizing against each expansion. They cannot leave the jurisdiction their security policy governs, and their leverage runs through elections and courts that have so far declined the question.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, antiwar_citizen_movements, payer,
    organized, biographical, trapped, national).

% Opposed the statutes and lost procedural battles over accelerated committee votes; their role in constitutional change is now limited to contesting exercises of an authority defined without them. Leaving the Diet would surrender the only platform they have.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, diet_minority_parties, payer,
    organized, biographical, constrained, national).

% Live near bases and training areas that expanded operations would activate; they bear noise, accident risk, and land burdens across generations. They were not consulted in the cabinet process and hold no seat in the interpretive debate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, host_community_residents, excluded,
    powerless, generational, trapped, local).

% Has declined to decide the constitutionality of the arrangement, treating it as a political question; its silence is load-bearing for the current settlement. It observes, takes cases selectively, and could reshape the structure with a ruling it has so far avoided.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, supreme_court_of_japan, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, japanese_cabinet).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared trigger standard telling SDF commanders, allied planners, and Diet overseers when collective force is lawful, replacing case-by-case improvisation with a single authorization test and enabling combined operational planning.
% TRANSFER_FUNCTION: Moves war-authority discretion from the constitutional text and the people's amendment prerogative to the cabinet; moves logistical obligations and operational risk outward onto allied operations and SDF personnel; moves procurement demand to the defense industrial base.
% ABSENT_VOICES: Host-community residents near bases, SDF enlisted personnel who bear deployment risk without a seat in trigger definition, and the referendum-holding public whose amendment prerogative was bypassed would all object to executive-authored expansion; they sit outside the cabinet-legislation-bureau channel where the reading was produced.
% DISAPPEARANCE_RATIONALE: If the collective-self-defense authorization vanished overnight, alliance planning would rearrange around the loss of Japanese vessel protection and logistics, SDF mission envelopes would contract to the individual-defense baseline, the 2015 statutes would lose their authorization basis, the interpretive precedent chain would break, and procurement programs justified by the expanded posture would lose their rationale.
% FOUNDING_PROBLEM: Postwar Japan faced a security dilemma: a pacifist charter coexisting with an exposed archipelago and an ally expecting reciprocity. The specific gap this reading addresses is that deterrence fails if Japan cannot defend allied forces operating in its vicinity.
% FOUNDING_PROBLEM_CORROBORATION: Independent security-studies literature and allied government planning documents attest the allied-defense gap is real. Constitutional scholars outside the benefiting parties corroborate that the problem exists while disputing that cabinet reinterpretation solves it legitimately: corroboration of the problem is broad, corroboration of the method is contested.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64: the authorization standard is a real coordination good, but the same instrument moves constituent-level authority (when Japan fights) from the amendment process to the cabinet, and the trigger's elasticity has absorbed every proposed expansion so far. Suppression 0.57: no coercion of persons, but institutional suppression is documented — the Cabinet Legislation Bureau's leadership was reshaped after it resisted the 2014 reversal, scholarly dissent was managed through appointment and funding channels, and the 2015 statutes passed after accelerated committee procedures. Theater ratio 0.44: the three-condition test has never been formally invoked to authorize combat, while the enabling statutes and procurement programs do the operative work — the displayed trigger increasingly performs legitimation rather than limitation. Accessibility collapse 0.38: formal amendment, litigation, and electoral reversal all remain open paths, so alternatives persist. Resistance 0.62: the 2015 protest cycle was the largest in decades, hundreds of scholars signed unconstitutionality statements, and minority parties forced procedural delays. Measurements run on one shared grid (t0 = 2014 cabinet decision, t12 = 2026, two-year steps); the final points are projections from the 2022 strategic-document trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the cabinet seat the arrangement computes as adaptive modernization: the text unchanged, the threat real, the method the only feasible one. From the professoriate and movement seats the same structure computes as usurpation: a transfer of constituent power executed without referendum, court, or supermajority. The divergence tracks exit structure — the cabinet is bound by its own precedent but controls the machinery; the professoriate is identity-locked to doctrinal integrity; movements are trapped in the jurisdiction. Coalition potential among the payer seats (scholars, opposition parties, movements) is real but has so far produced obstruction, not reversal.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the cabinet (author and collector, nearest the beneficiary end), US alliance planners (subsidized from outside the polity), the SDF officer corps (institutional gain, partly offset by operational risk the derivation may underweight), and the procurement sector. Victim declarations map to high directionality: the professoriate (identity-locked, so effective extraction is amplified), antiwar movements and minority parties (trapped/constrained), and host communities (trapped and excluded from the formal roles entirely). Scope is national with regional operational reach; verifying compliance with the trigger is hard precisely because the trigger is interpretive, which pushes effective extraction upward for target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an allied-defense gap inside a war-renouncing charter — is live, so this is not a mandate outliving its function; mandatrophy_resolved stays undeclared. The live risk is the opposite drift: elasticity converting coordination into accumulation, with the displayed trigger decoupling from operative authorization (rising theater_ratio) while extraction accumulates (rising base_extractiveness). Classifying the arrangement as tangled_rope keeps the genuine coordination core visible — mislabeling it pure extraction would erase the real authorization and alliance-interoperability function — while the enforcement requirement and victim declarations register the asymmetry that a pure-coordination label would hide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_article_9,
    'This constraint is one reading of the article_9_war_renunciation kernel — how would the sibling readings restructure the beneficiary/victim surface?',
    'Comparative analysis across the three reading files; the declared foreclosure edge to the strict-pacifist reading and influence edge to the inherent-right reading carry the structural deltas.',
    'Under the strict-pacifist reading the entire standing force is unauthorized and the victim set expands to all SDF personnel and their commanders; under the inherent-right reading missions cap at individual defense and the overseas/allied scope closes, removing the US-planner and procurement beneficiary seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_article_9, conceptual, 'Committer structure: one kernel, three readings, disagreement located in clause reach and interpretive authority.').

omega_variable(
    elasticity_ratchet_unboundedness,
    'Does the survival-threatened trigger admit principled stopping points, or does each crisis redefine the threshold upward?',
    'Track trigger invocations and capability justifications across successive crises; test whether any proposed expansion has been refused on trigger grounds.',
    'If unbounded, the interpretive layer functions as a standing license, effective extraction trends toward the full-target end for constitutional-process seats, and the classification drifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elasticity_ratchet_unboundedness, empirical, 'Whether the elastic trigger is bounded by principle or ratchets without limit.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is cabinet-level reinterpretation a legitimate mode of constitutional change absent judicial confirmation or popular amendment?',
    'Comparative constitutional analysis of Westminster-style interpretive conventions, domestic litigation outcomes, and public-reason argumentation on constituent power.',
    'If illegitimate, the transfer of constituent power to the executive is uncompensated extraction and epsilon rises sharply; if legitimate within Japan''s governing conventions, part of the measured extraction is ordinary constitutional evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Legitimacy of the interpretive channel itself, independent of the substantive outcome.').

omega_variable(
    trigger_realism_threat_environment,
    'Are scenarios meeting the survival-threatened condition realistically probable, or does the trigger operate as a standing authorization detached from likely events?',
    'Net assessment of regional threat scenarios, wargaming, and comparison of declared triggers against intelligence assessments.',
    'If detached, the coordination function thins toward cover and the theater ratio understates performative maintenance; if real, part of the arrangement''s cost is genuine insurance against a live gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_realism_threat_environment, empirical, 'Whether the trigger corresponds to probable events or floats free of them.').

omega_variable(
    alliance_entrapment_risk_allocation,
    'Does the arrangement allocate entrapment risk onto SDF personnel and the public without corresponding consent mechanisms?',
    'Examine consultation procedures, prior-consent requirements for operations in combat zones, and alliance command arrangements.',
    'If entrapment risk is real and unconsented, victim-side extraction exceeds the measured profile and coalition potential among affected seats rises materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_entrapment_risk_allocation, empirical, 'Distribution of drawn-in war risk versus consent across the payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t2, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2, 0.33).
narrative_ontology:measurement_basis(arti_tr_t2, observed).
narrative_ontology:measurement(arti_tr_t4, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t6, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(arti_tr_t6, observed).
narrative_ontology:measurement(arti_tr_t8, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t12, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement_basis(arti_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t2, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2, 0.51).
narrative_ontology:measurement_basis(arti_be_t2, observed).
narrative_ontology:measurement(arti_be_t4, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t6, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(arti_be_t6, observed).
narrative_ontology:measurement(arti_be_t8, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t12, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(arti_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t2, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement_basis(arti_su_t2, observed).
narrative_ontology:measurement(arti_su_t4, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t6, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(arti_su_t6, observed).
narrative_ontology:measurement(arti_su_t8, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t12, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(arti_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article 9' conflates three structurally distinct constraints — one per reading of the fixed text. The strict-pacifist reading is the historical baseline (1945 onward); the inherent-right reading emerged with the SDF's creation (1954) and grounds the minimum-defense settlement; this collective-self-defense reading is the downstream 2014 expansion whose elasticity feeds on the stability of the inherent-right settlement. Each file carries its own epsilon, beneficiary/victim structure, and classification; this file links both siblings via affects_constraints, and the sibling files should carry reciprocal links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
