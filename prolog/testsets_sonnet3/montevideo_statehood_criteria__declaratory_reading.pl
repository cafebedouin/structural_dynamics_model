% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Declaratory Reading of the Montevideo Statehood Criteria
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The declaratory reading holds that an entity satisfying the Montevideo
 *   criteria is a state as a matter of international law, full stop —
 *   recognition by other states is merely evidentiary, not constitutive. This
 *   reading was designed to prevent statehood from being a gift dispensed at
 *   the pleasure of established powers. In practice, the reading functions as
 *   a tangled rope: it genuinely coordinates expectations around an objective
 *   checklist (the coordination function), but the practical incidents of
 *   statehood — UN membership, treaty capacity, diplomatic immunity, access
 *   to international financial systems — remain gated by the very recognition
 *   the doctrine claims is non-constitutive. Entities that meet the criteria
 *   (Somaliland, historically Taiwan, arguably Abkhazia/South Ossetia
 *   depending on the observer) are told they are legally states while being
 *   denied nearly every practical capacity of statehood. The doctrine's own
 *   beneficiaries (entities meeting the criteria) become victims of the gap
 *   between the doctrine's promise and its enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.42).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.55).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Declaratory Reading of the Montevideo Statehood Criteria").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '908b970f-d625-477d-996c-e3a25584fba0').
narrative_ontology:cs_kernel_codification('908b970f-d625-477d-996c-e3a25584fba0', formalized).
narrative_ontology:cs_authority_grounding('908b970f-d625-477d-996c-e3a25584fba0', distributed).
narrative_ontology:cs_reading_relation('908b970f-d625-477d-996c-e3a25584fba0', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('908b970f-d625-477d-996c-e3a25584fba0', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('908b970f-d625-477d-996c-e3a25584fba0', foundational, statehood_is_self_executing_upon_criteria_satisfaction).
narrative_ontology:cs_axiom_status(statehood_is_self_executing_upon_criteria_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('908b970f-d625-477d-996c-e3a25584fba0', statehood_is_self_executing_upon_criteria_satisfaction, conventional).
narrative_ontology:cs_axiom('908b970f-d625-477d-996c-e3a25584fba0', secondary, recognition_is_merely_evidentiary_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_merely_evidentiary_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('908b970f-d625-477d-996c-e3a25584fba0', recognition_is_merely_evidentiary_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('908b970f-d625-477d-996c-e3a25584fba0', montevideo_convention_1933_baseline).
narrative_ontology:cs_drift_state('908b970f-d625-477d-996c-e3a25584fba0', contemporary_post_kosovo_advisory_opinion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('908b970f-d625-477d-996c-e3a25584fba0', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_seeking_statehood).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, small_and_new_states_generally).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, unrecognized_de_facto_authorities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, populations_of_contested_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_seeking_statehood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls a defined territory and permanent population, exercises effective government, and claims capacity to enter relations with other states. Under the declaratory reading, this entity is already a state as a matter of law the moment the four criteria are met, regardless of whether other states extend recognition. Its 'benefit' is legal — the doctrine validates its claim on paper — but in practice it still cannot access UN membership, treaty regimes, or foreign courts without recognition, so the doctrine's promise and its practical situation diverge.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_seeking_statehood, beneficiary,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_seeking_statehood, payer).

% Meets the objective criteria by any reasonable measure but is denied recognition by powerful states for geopolitical reasons. Under the declaratory reading this entity is told it is legally a state, yet it cannot bank internationally, cannot sue or be sued as a sovereign, cannot join most treaties, and cannot defend its territory through international legal channels that presuppose recognized statehood. The doctrine's declaratory promise becomes a source of false hope and diplomatic isolation rather than actual capacity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, unrecognized_de_facto_authorities, payer,
    powerless, generational, trapped, national).

% Live under a government that satisfies the objective criteria but lacks broad recognition. Denied access to international aid channels, freedom of movement instruments, and legal protections that flow through recognized-state status, despite living under a functioning government by any factual measure. Cannot exit the territory's legal limbo; their day-to-day welfare is hostage to a doctrinal dispute they did not create.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, populations_of_contested_territories, payer,
    powerless, biographical, trapped, local).

% Benefit from a legal order in which their statehood does not depend on securing case-by-case approval from great powers; the declaratory doctrine, when it holds, protects newly formed or historically weak states from being permanently vetoed out of legal existence by rivals or former colonial powers.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, small_and_new_states_generally, beneficiary,
    moderate, civilizational, constrained, global).

% Existing states — especially powerful ones and former parent states — control the practical machinery (UN admission, diplomatic relations, treaty access, financial systems) that actually operationalizes statehood, regardless of what the declaratory doctrine says on paper. They can withhold recognition from an entity that meets every objective criterion, effectively vetoing legal statehood in practice while conceding it in theory. This is the enforcement lever that keeps the doctrine's practical bite limited to what recognizers permit.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states_and_powerful_recognizers, agenda_setter,
    institutional, generational, arbitrage, global).

% Interpret and apply the Montevideo criteria in scholarship, arbitration, and advisory opinions; document the persistent gap between declaratory theory and constitutive practice; do not control outcomes but shape which reading dominant institutions cite.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_scholars_and_tribunals, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, diffuse).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, objective checklist (territory, population, government, capacity for relations) so that statehood can in principle be assessed by fact rather than by the unilateral political preference of any single existing state — a genuine attempt to prevent statehood from being purely a gift of the powerful.
% TRANSFER_FUNCTION: In theory the doctrine transfers legal status automatically to any entity meeting the criteria. In practice, because recognition still gates nearly all practical incidents of statehood (UN seat, treaties, banking, diplomatic immunity), the doctrine transfers legitimacy rhetoric to entities that satisfy the criteria while the actual capacities of statehood remain gated by whoever controls recognition — a gap that primarily costs the unrecognized entity and its population.
% ABSENT_VOICES: Unrecognized de facto authorities and their populations have no formal seat at the table where recognition decisions are made — they can assert they meet the criteria, but the forums that matter (UN Security Council, individual state foreign ministries) are controlled by already-recognized states with their own interests.
% DISAPPEARANCE_RATIONALE: If the declaratory reading disappeared and only the constitutive reading remained, recognized states would lose no formal power they do not already exercise in practice (since recognition already gates real-world capacity) — but de facto authorities meeting the objective criteria would lose even the rhetorical and legal argument they currently use in international forums, arbitration, and advocacy. Scholars and some smaller states would say the world changes meaningfully (a normative floor disappears); powerful recognizing states would say little changes because they already control the practical levers regardless of doctrine.
% FOUNDING_PROBLEM: Early 20th-century international law needed a way to determine statehood that did not make every new state's existence permanently hostage to the political whims of established powers — the 1933 Montevideo Convention articulated objective criteria partly as a reaction against purely discretionary, often colonially-inflected recognition practices.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and the drafting history of the Montevideo Convention itself attest the founding problem (arbitrary discretionary recognition) was real and partially addressed. However, state practice since 1933 — Kosovo, Taiwan, Somaliland, Abkhazia, South Ossetia, Palestine — is cited by scholars, UN documentation, and international tribunals (outside the entities that would benefit from declaratory status) as evidence that recognition remains the operative gate in practice, meaning the founding problem the doctrine claims to solve persists largely unresolved despite the doctrine's formal existence.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, contested).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).
:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at present) because the doctrine does not extract resources directly — its damage is the false promise of self-executing legal status that recognized states can simply ignore. Suppression is higher (0.55) and has risen over the interval as state practice has hardened around treating recognition as the operative gate regardless of doctrinal statements, meaning the objective criteria function increasingly as rhetorical cover for what is actually a recognition-gated system. Theater ratio (0.30) reflects that scholars, tribunals, and even some states continue to invoke the declaratory language in formal contexts (ICJ advisory opinions, academic commentary) even as practical outcomes track constitutive logic — a genuine but partially decorative doctrinal commitment.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an entity meeting the Montevideo criteria, the declaratory reading is a genuine legal victory — a shield against arbitrary non-recognition. From the seat of a powerful recognizing state, the same doctrine is a dead letter it can invoke or ignore at will, because nothing compels it to act as though a merely-declared state actually exists in any forum that state controls. The engine should compute these seats differently: the declaratory reading nominally serves the powerless (coordination against discretionary veto) but its enforcement is entirely in the hands of the powerful, producing the tangled-rope signature rather than a clean rope.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities meeting the criteria are simultaneously named as beneficiaries (the doctrine validates their claim) and payers (the doctrine's promise is not enforced against powerful recognizers, so they bear the cost of unmet expectations). Unrecognized authorities and their populations sit closer to pure victim: they satisfy the doctrine's own test yet receive none of statehood's practical benefits, and their exit options are trapped — they cannot simply relocate their claim to a more favorable forum. Parent states and powerful recognizers hold agenda-setting power because, whatever the declaratory doctrine says, they control the practical machinery (UN admission votes, bilateral recognition, treaty accession, SWIFT/banking access) that makes statehood operative — this is the asymmetric enforcement lever a tangled rope requires.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing statehood from being purely discretionary — remains partially live (arbitrary non-recognition still occurs) but the mechanism built to solve it (self-executing legal fact upon meeting criteria) has been substantially hollowed by seven decades of state practice that treats recognition as operative regardless of doctrine. This is not full mandatrophy (the doctrine still shapes argument and occasionally outcomes, e.g., in ICJ reasoning) but it sits close to it: the doctrine persists more as an argumentative resource for weak claimants than as a mechanism that reliably delivers what it promises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_practical_efficacy,
    'Does the declaratory reading meaningfully constrain powerful states'' recognition behavior in practice, or has it become purely rhetorical scaffolding that recognition-gated practice has hollowed out?',
    'Track cases where an entity meeting the Montevideo criteria (Somaliland, Taiwan pre-1970s, Republic of China post-1971, Abkhazia) achieved practical incidents of statehood (UN-adjacent participation, treaty capacity, foreign banking access) absent broad recognition, versus cases where meeting the criteria produced no practical change absent recognition.',
    'If the declaratory reading never produces practical statehood absent recognition, the doctrine functions closer to pure theater dressed as tangled rope, and the classification should drift toward piton (form persists, function atrophied) rather than active tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_practical_efficacy, empirical, 'Whether declaratory statehood produces real practical capacity or only rhetorical standing.').

omega_variable(
    kernel_reading_choice_and_scope,
    'Is the declaratory reading the correct lens for THIS constraint, or does the disagreement between declaratory, constitutive, and hybrid readings reflect that ''the Montevideo criteria'' names three structurally distinct legal claims that should never be scored on one ε?',
    'This story deliberately decomposes the kernel into three separate constraint files (declaratory_reading, constitutive_reading, hybrid_reading), each with its own ε, beneficiaries, victims, and classification, linked via network.affects_constraints — per the ε-invariance principle.',
    'Confirms this file''s ε (0.42) is a property of the declaratory reading specifically; the constitutive_reading sibling would likely show materially different beneficiary structure (recognized powerful states as clearer beneficiaries, unrecognized entities as clearer victims) and possibly higher ε given its more explicit gatekeeping function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_choice_and_scope, conceptual, 'Documents the kernel decomposition rationale and the location of reading disagreement.').

omega_variable(
    recognition_as_political_weapon,
    'When powerful states withhold recognition from an entity meeting the objective criteria for geopolitical reasons unrelated to the criteria themselves (e.g., alliance politics, precedent-setting fears), is this a corruption of the declaratory doctrine or evidence the doctrine never had real force?',
    'Comparative analysis of recognition decisions against stated versus actual rationales — do recognizing states cite the Montevideo criteria in their recognition/non-recognition reasoning, or do they cite unrelated political considerations while nominally accepting the doctrine?',
    'If political considerations dominate and criteria-based reasoning is largely post-hoc justification, this supports treating the declaratory reading''s coordination function as substantially theatrical rather than operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_as_political_weapon, conceptual, 'Whether stated criteria-based reasoning tracks or merely rationalizes political recognition decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement(mont_tr_t1955, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1975, 0.24).
narrative_ontology:measurement(mont_tr_t1995, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.25).
narrative_ontology:measurement(mont_be_t1955, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1955, 0.3).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(mont_be_t1995, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.35).
narrative_ontology:measurement(mont_su_t1955, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1955, 0.42).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement(mont_su_t1995, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1995, 0.51).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__declaratory_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the montevideo_statehood_criteria kernel. The declaratory_reading (this file) holds that meeting the four objective criteria is dispositive; the constitutive_reading holds that recognition by existing states is what creates statehood as a legal fact; the hybrid_reading adds normative legitimacy conditions (democratic governance, human rights compliance, non-aggression) atop the objective criteria. Each reading produces a different beneficiary/victim structure and a different ε, because each reading licenses a different real-world practice: declaratory reading licenses unilateral self-declaration claims; constitutive reading licenses recognition-withholding as a legitimate veto; hybrid reading licenses conditionality regimes that gate recognition on normative compliance. The three are linked here rather than merged because they are structurally distinct legal claims, not three measurements of one claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
