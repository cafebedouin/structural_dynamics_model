% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Agreement Article 4 NDC Mechanism — Sovereigntist Reading
 *   domain: international climate governance / treaty law / political economy
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Paris Agreement
 *   Article 4 kernel: NDCs are voluntary, self-determined pledges that
 *   preserve national energy sovereignty. Under this reading, the mechanism's
 *   low epsilon reflects that no state cedes binding authority over its
 *   domestic energy trajectory — the pledge-and-review cycle is a
 *   coordination device for comparable, public commitment-making among states
 *   that would not accept external bindingness, not an extraction mechanism.
 *   This is a DIFFERENT constraint from the supranational reading (which
 *   reads Article 4 as a ratcheting binding trajectory with accountability)
 *   and the equity reading (which reads it through Common But Differentiated
 *   Responsibilities requiring structural developed/developing distinctions)
 *   — same treaty text, three structurally distinct constraints per the
 *   ε-invariance principle. The three readings are linked via
 *   network.affects_constraints and share the kernel_id paris_article_4_ndc;
 *   each authors its own ε independently.
 *
 * KEY AGENTS:
 *   - fossil_dependent_developing_states: primary beneficiary (moderate/mobile) — retains development-pathway sequencing
 *   - major_emitter_states_with_domestic_political_constraints: primary beneficiary and agenda-setter (institutional/arbitrage) — negotiates own pledge text, can exit and re-enter
 *   - national_energy_sector_incumbents: secondary beneficiary (organized/constrained) — benefits from domestic rather than international rule-setting
 *   - small_island_and_low_lying_states: excluded voice (powerless/trapped) — bears aggregate climate exposure without pledge-revision leverage
 *   - unfccc_secretariat_and_review_bodies: analytical observer (institutional/analytical) — aggregates and reports, does not adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.12).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Agreement Article 4 NDC Mechanism — Sovereigntist Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international climate governance / treaty law / political economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '3e16d53e-4e88-4a86-84e0-568c5a8f49ce').
narrative_ontology:cs_kernel_codification('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', formalized).
narrative_ontology:cs_authority_grounding('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', distributed).
narrative_ontology:cs_reading_relation('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', foundational, state_energy_sovereignty_precedes_external_ratchet).
narrative_ontology:cs_axiom_status(state_energy_sovereignty_precedes_external_ratchet, holdable).
narrative_ontology:cs_axiom_grounding('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', state_energy_sovereignty_precedes_external_ratchet, conventional).
narrative_ontology:cs_axiom('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', secondary, voluntary_universal_participation_outperforms_binding_partial_participation).
narrative_ontology:cs_axiom_status(voluntary_universal_participation_outperforms_binding_partial_participation, holdable).
narrative_ontology:cs_axiom_grounding('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', voluntary_universal_participation_outperforms_binding_partial_participation, instrumental).
narrative_ontology:cs_reference_frame('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', post_copenhagen_voluntary_pledge_settlement).
narrative_ontology:cs_drift_state('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', post_2023_global_stocktake, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3e16d53e-4e88-4a86-84e0-568c5a8f49ce', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, major_emitter_states_with_domestic_political_constraints).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, national_energy_sector_incumbents).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, state_sovereignty_over_domestic_energy_policy).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, bottom_up_pledge_and_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets its own NDC target and timeline without an external body able to compel a higher ambition or penalize a downward revision at the next cycle. Retains the ability to sequence coal or gas buildout against domestic development and electrification goals, and can revise its pledge downward at the next five-year cycle without formal sanction beyond reputational commentary.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_developing_states, beneficiary,
    moderate, generational, mobile, national).

% Negotiates and files its own pledge text, calibrated to what its domestic legislature or coalition can sustain; can also withdraw from or renegotiate the wider framework (as demonstrated by prior formal withdrawal and re-entry) without the treaty machinery itself collapsing. Uses the self-determined pledge structure to avoid ceding domestic energy and industrial policy to an external ratchet.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, major_emitter_states_with_domestic_political_constraints, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, major_emitter_states_with_domestic_political_constraints, agenda_setter).

% Fossil fuel, utility, and heavy-industry incumbents inside pledging states benefit from the absence of a binding external trajectory: national pledges are negotiated domestically, where incumbents have lobbying access, rather than imposed by an international body they cannot lobby directly.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, national_energy_sector_incumbents, beneficiary,
    organized, biographical, constrained, national).

% Face existential exposure to warming trajectories set by the aggregate of others' self-determined pledges but have no mechanism under this reading to compel higher ambition from major emitters; their preference for binding, ratcheting commitments is a live position in the negotiating room but is not the operative logic of this reading.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, small_island_and_low_lying_states, excluded,
    powerless, civilizational, trapped, global).

% Compiles, tracks, and reports on submitted NDCs and conducts the global stocktake, but under this reading holds no authority to reject, modify, or penalize a pledge for insufficient ambition — its function is procedural aggregation and transparency, not adjudication.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat_and_review_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common reporting format and five-year cycle so that roughly 190 states with radically different energy endowments, development stages, and political systems can each commit to something, publicly, on a comparable timetable — solving the problem that a single binding global emissions treaty had repeatedly failed to secure ratification (Kyoto's asymmetric bindingness, the 2009 Copenhagen impasse).
% TRANSFER_FUNCTION: Under this reading, the mechanism transfers very little materially between states — no binding finance, technology, or emissions-right transfer is compelled by the NDC structure itself. What it transfers is legitimacy and time: pledging states receive international recognition of good-faith participation and years of policy runway, in exchange for no enforceable obligation beyond periodic reporting.
% ABSENT_VOICES: Small island states, some African and South Asian delegations, and youth and future-generations advocates argue the self-determined structure lets the largest emitters set the de facto global ceiling too low; they are present in negotiating rooms but their preferred binding-ratchet framing (the supranational reading) is not the logic this reading operates on, and their disaggregated bargaining power cannot force pledge revision.
% DISAPPEARANCE_RATIONALE: Sovereigntist-reading advocates hold that if the voluntary, self-determined pledge structure disappeared and were replaced by binding external targets, several major-emitter states would exit the framework entirely (as historical withdrawal threats and actual withdrawal demonstrate), producing a worse aggregate outcome than universal-but-soft participation; supranational-reading advocates hold the opposite — that the world's emissions trajectory would barely change either way because the soft pledges were never binding on behavior. The verdict is genuinely disputed between the readings, not settled by this story.
% FOUNDING_PROBLEM: The 2009 Copenhagen summit's attempt at a binding top-down global target collapsed under the refusal of major emitters (developed and developing) to accept externally-set, legally binding caps; Paris Article 4 was built to secure near-universal participation by letting each state set its own pledge.
% FOUNDING_PROBLEM_CORROBORATION: Independent accounts from UNFCCC process historians and academic treaty-design scholars (outside any pledging state's government) corroborate that the shift to nationally-determined, bottom-up pledges was a direct, documented response to Copenhagen's binding-target failure, and that several major emitters' continued participation remains conditioned on the pledge structure remaining non-binding — this is attested in negotiating-history literature independent of any single state's self-interested framing.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because, by this reading's own lights, no party extracts rent or compels transfer through the NDC structure itself — the standing arrangement under contest is the voluntary pledge regime as the sovereigntist reading sees it, and that arrangement is genuinely low-coercion: no state is compelled to a target it did not choose, and no enforcement body can penalize a downward revision. Suppression is low (0.12) for the same reason — there is no coercive machinery compelling compliance beyond reputational pressure. Theater ratio is authored moderate-rising (0.30 to 0.45) because a growing share of the pledge-and-review apparatus (stocktakes, high-level dialogues, ambition summits) is genuinely procedural performance relative to the modest behavioral commitment underneath it — this is descriptively true independent of the low-extraction claim, and the divergence between rising theater and flat extraction is itself diagnostic: a coordination mechanism whose ceremonial layer is growing faster than its substantive bite.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil-dependent developing states and major emitters with domestic constraints sit near the beneficiary end (low d): the pledge structure subsidizes their preferred outcome (sovereignty over pace and sequencing) relative to a binding alternative. National energy incumbents benefit indirectly through domestic-only rule-setting access. Small island states are declared excluded rather than victim under THIS reading — the sovereigntist reading does not construct them as a victim class, because the reading's own logic holds that universal soft participation is the best achievable outcome; their exposure is real but is not attributed to this constraint's operation by this reading's lights. No victims are declared, consistent with the reading's low-epsilon assessment of its own referent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing near-universal participation after Copenhagen's binding-target collapse) is still live by the corroboration record: major emitters' continued participation remains conditioned on non-bindingness, and no independent process historian attests that this condition has lapsed. This blocks a premature mandatrophy finding — the rope function has not obviously outlived its purpose, even though rising theater_ratio invites scrutiny of whether the review machinery is doing more ceremony than coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_ratchet_framing_location,
    'Is the Paris Agreement''s core mechanism better characterized as a sovereignty-preserving coordination device (this reading) or a binding ratchet mechanism whose bindingness is currently under-enforced (the supranational reading)? Where exactly does the disagreement between these framings live — in the treaty text, in state practice, or in the review body''s evolving interpretation?',
    'Track whether the global stocktake process (2023, 2028, ongoing) develops binding consequences for insufficient ambition, or remains purely informational; a shift toward consequence-bearing review would support the supranational reading and undercut this one''s low-epsilon assessment.',
    'If the review mechanism hardens into de facto bindingness, this reading''s low-extraction assessment would no longer describe the operative arrangement, and the constraint this story describes would need re-authoring or would collapse toward the supranational reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_ratchet_framing_location, conceptual, 'Location of the sovereigntist/supranational disagreement: text vs. practice vs. review-body interpretation.').

omega_variable(
    exclusion_vs_victimhood_of_frontline_states,
    'Is the absence of small island and low-lying states'' preferred binding-ratchet outcome from this reading''s operative logic a genuine structural exclusion (they are present but powerless to change the mechanism) or does it amount to a victim relationship this reading declines to name?',
    'Compare aggregate warming trajectory implied by submitted NDCs under this reading against the trajectory the equity/supranational readings would require; if the gap between them tracks directly onto frontline-state existential exposure, the exclusion may be better modeled as victimhood in a sibling reading.',
    'If resolved toward victimhood, a sibling reading (not this one) would need to declare small_island_and_low_lying_states as a victim group with correspondingly higher epsilon — this reading''s own referent and low-epsilon claim would remain intact as one reading''s assessment, but the corpus would carry the tension explicitly across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_vs_victimhood_of_frontline_states, conceptual, 'Whether frontline-state exclusion under this reading is structurally distinct from victimhood, or a naming choice this reading makes.').

omega_variable(
    theater_ratio_trajectory_significance,
    'Does the rising theater_ratio (0.30 to 0.45) indicate the pledge-and-review apparatus is drifting toward pure ceremony (a piton-adjacent trajectory within the rope classification), or does it reflect legitimate growth in transparency infrastructure that itself has coordination value?',
    'Compare growth in stocktake/dialogue activity against measurable changes in aggregate pledged ambition; if procedural activity grows while aggregate ambition stagnates, this supports the ceremony-drift reading.',
    'A confirmed ceremony-drift finding would not change this story''s claimed_type by author fiat, but would strengthen the case for a T17-style abductive flag on the constraint''s trajectory even while ε remains low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_trajectory_significance, empirical, 'Whether rising theater ratio signals goodhart drift in the review process or legitimate transparency-infrastructure growth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2027, 0.44).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2030, 0.45).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2018, 0.14).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2021, 0.16).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2024, 0.17).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2027, 0.18).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2030, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(paris_article_4_ndc__sovereigntist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.1).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the paris_article_4_ndc kernel, decomposed per the ε-invariance principle: sovereigntist_reading (this file, low ε ~0.18, rope), supranational_reading (binding-ratchet framing, expected higher ε and enforcement-dependent structure), and equity_reading (CBDR-structured framing, expected differentiated beneficiary/victim structure along developed/developing lines). Each reading authors its own ε, beneficiaries, victims, and claimed_type independently from the same underlying treaty text (Paris Agreement Article 4). The readings are linked bidirectionally via affects_constraints to support contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
