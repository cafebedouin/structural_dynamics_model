% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitution — Parliamentary Constraint Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-constraint reading of the Fifth
 *   Republic kernel: the president is treated as a coordinated executive
 *   whose policy program requires the National Assembly's ongoing confidence
 *   and its affirmative legislative action to become law. On this reading,
 *   the constitutional text's confidence and censure mechanisms (Articles 49,
 *   50) are not vestigial — they are the load-bearing hinge by which the
 *   elected chamber can compel a change of government or block a presidential
 *   program outright. This is a distinct constraint from the
 *   hyper-presidential reading (which treats the president as the primary
 *   sovereign minimally checked by the chamber) and from the
 *   cohabitation-equilibrium reading (which treats authority as continuously
 *   negotiated between president and prime minister). Each reading has its
 *   own beneficiary/victim structure and its own epsilon; they are linked
 *   here only as siblings in the same kernel contest, not blended.
 *
 * KEY AGENTS:
 *   - national_assembly_majority: primary beneficiary and agenda-setter — can censure the government and block enabling legislation
 *   - president: primary target under this reading — policy program depends on a majority the president does not unilaterally control
 *   - government_ministers: secondary payer — tenure hostage to floor votes
 *   - opposition_coalition: secondary beneficiary — converts chamber arithmetic into veto leverage
 *   - citizens_and_civil_society: excluded — no direct seat in the confidence mechanism between elections
 *   - constitutional_council: analytical observer — adjudicates the boundary disputes this reading generates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.32).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution — Parliamentary Constraint Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '24c6878b-54e3-40f7-b915-94903fc58d8c').
narrative_ontology:cs_kernel_codification('24c6878b-54e3-40f7-b915-94903fc58d8c', formalized).
narrative_ontology:cs_authority_grounding('24c6878b-54e3-40f7-b915-94903fc58d8c', lineage).
narrative_ontology:cs_interpretation_layer_present('24c6878b-54e3-40f7-b915-94903fc58d8c').
narrative_ontology:cs_reading_relation('24c6878b-54e3-40f7-b915-94903fc58d8c', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('24c6878b-54e3-40f7-b915-94903fc58d8c', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('24c6878b-54e3-40f7-b915-94903fc58d8c', foundational, executive_derives_mandate_from_assembly_confidence).
narrative_ontology:cs_axiom_status(executive_derives_mandate_from_assembly_confidence, holdable).
narrative_ontology:cs_axiom_grounding('24c6878b-54e3-40f7-b915-94903fc58d8c', executive_derives_mandate_from_assembly_confidence, conventional).
narrative_ontology:cs_axiom('24c6878b-54e3-40f7-b915-94903fc58d8c', secondary, censure_power_is_a_live_structural_check).
narrative_ontology:cs_axiom_status(censure_power_is_a_live_structural_check, holdable).
narrative_ontology:cs_axiom_grounding('24c6878b-54e3-40f7-b915-94903fc58d8c', censure_power_is_a_live_structural_check, empirically_contingent).
narrative_ontology:cs_reference_frame('24c6878b-54e3-40f7-b915-94903fc58d8c', parliamentary_responsible_government).
narrative_ontology:cs_drift_state('24c6878b-54e3-40f7-b915-94903fc58d8c', post_cohabitation_direct_election_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24c6878b-54e3-40f7-b915-94903fc58d8c', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, opposition_coalition_when_censuring).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_when_confidence_withheld).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, government_ministers_facing_censure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, opposition_coalition).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, government_ministers).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, responsible_government_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes needed to pass or block the government's legislative program, to censure the government (Article 49), and to withhold confidence. Can force a government resignation and can reshape or reject presidential policy priorities that require statutory implementation. Its members retain their own electoral mandate independent of the president's and can realign coalitions between elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, beneficiary).

% Under this reading, the president sets broad direction but depends on the Assembly and the government it can topple to translate policy into enacted law. When the Assembly withholds confidence in the government or votes down enabling legislation, the president's program stalls regardless of personal popularity. Exit is constrained: dissolution power exists but is limited (once per year, and politically risky), and a hostile majority can persist across a presidential term.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    powerful, biographical, constrained, national).

% Serve at the pleasure of a majority they do not control; a successful motion of censure under Article 49 forces collective resignation regardless of individual ministerial performance. They administer policy but cannot unilaterally guarantee its survival, since their tenure is hostage to a floor vote they do not chair.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, government_ministers, payer,
    moderate, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, government_ministers, excluded).

% Uses censure motions and legislative blocking power as leverage against the executive's program. Benefits directly whenever the constraint is invoked, since it converts numerical strength in the chamber into policy veto power without needing to control the presidency itself.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, opposition_coalition, beneficiary,
    organized, biographical, mobile, national).

% Experience the consequences of gridlock or negotiated compromise between assembly and executive but have no direct seat in the confidence mechanism between elections. Their recourse is the ballot at the next legislative or presidential election, not the intervening period.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens_and_civil_society, excluded,
    powerless, generational, trapped, national).

% Adjudicates disputes about the proper allocation of authority between assembly and executive when constitutional questions arise, without itself holding a stake in any particular outcome.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that executive policy carries a durable legislative mandate rather than resting solely on presidential will — coordinating the executive's program with the elected chamber that must vote the enabling statutes and budget.
% TRANSFER_FUNCTION: Moves effective policy-making leverage from the presidency to the Assembly majority whenever the Assembly withholds confidence or refuses to pass enabling legislation; the executive's agenda is transferred, in whole or in part, into the Assembly's discretion.
% ABSENT_VOICES: Citizens affected by resulting policy gridlock or forced compromise have no direct procedural voice in the confidence mechanism itself; their only leverage is the next election, which arrives after the relevant governing period has elapsed.
% DISAPPEARANCE_RATIONALE: If the Assembly's confidence and legislative-authorization power over the executive disappeared, the president could implement policy by decree-like assertion without needing to secure a parliamentary majority — governments would no longer fall to censure motions, ministerial tenure would delink from chamber support, and the entire coalition-bargaining apparatus of French parliamentary politics would cease to matter.
% FOUNDING_PROBLEM: The Fourth Republic collapsed partly from a chronically unstable executive at the mercy of shifting parliamentary coalitions; the 1958 Constitution was built to strengthen the executive while still requiring it to answer to an elected chamber, avoiding both legislative paralysis and unchecked personal rule.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative-government analysts outside any sitting government attest that the confidence and censure mechanisms remain a live, structurally load-bearing constraint on the executive (most visibly during cohabitation periods and hung-parliament episodes); presidential-camp actors and some Fifth Republic historians argue the mechanism has been substantially hollowed out by direct presidential legitimacy from popular election and by rare, controlled use of Article 49.3, making the 'coordinated executive' framing partly a legal fiction in ordinary majority government.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, under this reading, the constraint's operation channels power toward an elected, accountable body (the Assembly majority) rather than toward a narrow rent-collecting faction — this is closer to coordination than extraction, though not zero, since the executive and its ministers do bear a real, sometimes severe, cost when confidence is withheld. Suppression is moderate (0.32): the mechanism is coercive in the sense that a government MUST fall on a lost confidence vote, but the coercion runs through an electorally accountable chamber, not through an unaccountable enforcer. The temporal series shows extraction and suppression rising through the 1986 and 1997 cohabitation periods (when a hostile majority actually exercised the constraint against a president of the opposite camp) and falling back after 2002 electoral synchronization reduced the frequency of hostile majorities, then rising again after 2022 when the presidency lost its outright majority.
 *
 * DIRECTIONALITY LOGIC:
 *   The Assembly majority is the structural beneficiary here: it collects real policy leverage through the confidence and legislative-authorization mechanism without itself bearing the ministerial accountability that comes from running the executive. The president and government ministers are the structural targets: their capacity to implement policy is contingent on a chamber majority they must build and hold, and when that majority is lost or withdrawn, their program (or their office, for ministers) collapses. This is the opposite directionality from the hyper-presidential reading, where the president would sit near the beneficiary end and the Assembly's formal powers would be read as largely decorative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic executive instability from shifting, undisciplined parliamentary coalitions — is contested as either still live or substantially resolved. If one holds that direct presidential election and rare but decisive tools like Article 49.3 have made the confidence mechanism mostly formal in ordinary majority government, this reading risks mandatrophy: the coordination story (chamber holds the executive accountable) persists as legal architecture while day-to-day practice has drifted toward the hyper-presidential pattern. The classification here is deliberately conservative (rope, not tangled_rope) because the mechanism does still bind decisively during hung-parliament and cohabitation periods — the 2022-2024 period shows it reasserting itself. Whether this reading's coordination function is 'live' or 'a shell that fires only in edge cases' is exactly the sixth-question genealogy dispute captured in founding_problem_status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_ambiguity,
    'Does the Fifth Republic''s actual operating practice track the parliamentary-constraint reading (executive bound by chamber confidence), the hyper-presidential reading (chamber formally empowered but practically deferential), or does it vary by period such that no single reading is stably correct?',
    'Comparative analysis of censure-motion outcomes, Article 49.3 usage frequency and consequence, and legislative amendment rates across presidencies with and without a disciplined Assembly majority.',
    'If practice consistently favors the hyper-presidential pattern except during hung parliaments and cohabitation, the parliamentary-constraint reading is better modeled as a latent constraint that activates only under specific electoral configurations rather than a continuously operative one — which would push its effective classification toward scaffold (conditional, not steady-state) rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, conceptual, 'Whether this reading describes the constitution''s steady state or only its edge-case behavior.').

omega_variable(
    confidence_mechanism_erosion,
    'Has repeated use of Article 49.3 to force legislation through without a full floor vote (bypassing amendment debate while still technically requiring the Assembly''s non-censure) hollowed out the coordination function this reading depends on?',
    'Track the ratio of major legislation passed via ordinary vote versus via 49.3-forced adoption over successive legislatures; a rising 49.3 ratio with stable non-censure outcomes would indicate the mechanism has become largely theatrical.',
    'If the coordination function has been substantially routed around via 49.3 while nominally ''requiring'' Assembly non-objection, the low theater_ratio authored here (0.22) may understate the degree to which the constraint has drifted toward performance — this would support reclassification toward piton for the specific sub-mechanism of ordinary legislative passage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confidence_mechanism_erosion, empirical, 'Whether Article 49.3 usage has converted genuine legislative authorization into a formality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(fift_tr_t1974, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1974, 0.16).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1997, 0.19).
narrative_ontology:measurement(fift_tr_t2008, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(fift_tr_t2022, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement(fift_be_t1974, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1974, 0.2).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.32).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(fift_be_t2008, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2008, 0.24).
narrative_ontology:measurement(fift_be_t2022, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2022, 0.3).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(fift_su_t1974, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1974, 0.26).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1997, 0.42).
narrative_ontology:measurement(fift_su_t2008, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2008, 0.28).
narrative_ontology:measurement(fift_su_t2022, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the fifth_republic_constitution kernel. hyper_presidential_reading assigns the opposite beneficiary/victim structure to the same constitutional text (president as beneficiary, Assembly's formal powers as largely decorative). cohabitation_equilibrium_reading treats the allocation as continuously negotiated rather than settled toward either pole, and applies specifically during divided-government periods. All three share the same underlying constitutional kernel but instantiate structurally distinct constraints with different epsilon values and different victim sets; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
