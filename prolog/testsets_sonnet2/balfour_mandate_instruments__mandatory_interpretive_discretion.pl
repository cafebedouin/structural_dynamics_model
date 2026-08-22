% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: Mandatory Power's Unreviewable Interpretive Discretion Over Mandate Instruments
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This story isolates the third reading of the Balfour/mandate kernel: not
 *   a claim about what the mandate text SAYS (national home primacy or
 *   dual-obligation indigenous rights are the sibling stories that make
 *   substantive readings), but a claim about WHO gets to decide between those
 *   readings and under what review. The mandatory power's discretion is here
 *   treated as the operative constraint in its own right — a meta-level
 *   arrangement that sits above the contested substantive text and determines
 *   which substantive reading gets operationalized at any given moment, with
 *   no external body able to fix the answer. The 1920 and 1940 land regimes
 *   and the 1922/1930/1939 White Papers are evidence of this discretion in
 *   motion, not evidence for either substantive reading alone.
 *
 * KEY AGENTS:
 *   - british_colonial_administration: agenda_setter/beneficiary (institutional/arbitrage) — holds and exercises unreviewable interpretive authority
 *   - arab_population_of_palestine: payer (organized/trapped) — bears strategic uncertainty from policy oscillation with no textual anchor
 *   - zionist_yishuv_leadership: payer (organized/constrained) — invests under one reading, faces reversal under the next
 *   - permanent_mandates_commission: excluded (institutional/analytical) — formally supervises, cannot bind
 *   - historians_of_mandate_palestine: observer (analytical/analytical) — assesses the record after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "Mandatory Power's Unreviewable Interpretive Discretion Over Mandate Instruments").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00').
narrative_ontology:cs_kernel_codification('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', fixed_text).
narrative_ontology:cs_authority_grounding('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', extraction).
narrative_ontology:cs_interpretation_layer_present('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00').
narrative_ontology:cs_reading_relation('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', foundational, adjudicative_authority_requires_no_external_ratification).
narrative_ontology:cs_axiom_status(adjudicative_authority_requires_no_external_ratification, holdable).
narrative_ontology:cs_axiom_grounding('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', adjudicative_authority_requires_no_external_ratification, conventional).
narrative_ontology:cs_axiom('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', secondary, interpretive_flexibility_is_administrative_necessity).
narrative_ontology:cs_axiom_status(interpretive_flexibility_is_administrative_necessity, holdable).
narrative_ontology:cs_axiom_grounding('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', interpretive_flexibility_is_administrative_necessity, instrumental).
narrative_ontology:cs_reference_frame('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', league_mandate_supervisory_framework).
narrative_ontology:cs_drift_state('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', arab_revolt_and_1939_white_paper_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bdbb54e3-0d5a-4fdd-b4ae-d903ba156d00', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_population_of_palestine).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_yishuv_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the mandate from the League of Nations and administers Palestine directly. Issues White Papers (1922, 1930, 1939) and land regulation ordinances (1920, 1940) that reinterpret the 'national home' and 'existing rights' clauses of the mandate instrument as strategic circumstances shift — Arab revolt, Jewish immigration pressure, Nazi persecution, imperial security needs in the approach to war. No external body reviews these reinterpretations; the League's Permanent Mandates Commission can question but not reverse them. Each reversal resets the bargaining baseline for both communities, preserving the administration's freedom of maneuver.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration, beneficiary).

% Comprises the demographic majority with claims to existing civil and political rights under the mandate's own text, but has no mechanism to compel Britain to honor any given interpretation once issued. The 1930 Passfield White Paper restricts land transfer and immigration in their favor; the 1939 White Paper does more so; yet neither is binding law immune from future reversal, and the 1920s land regime is loosened again when strategic calculus shifts. Cannot appeal outside the mandatory power's own institutions to lock in a favorable reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_population_of_palestine, payer,
    organized, generational, trapped, regional).

% Builds institutions and pursues immigration and land purchase under the 1917 Balfour Declaration's incorporation into the mandate text, investing on the assumption that 'national home' entails demographic transformation. Faces the same reversal risk: the 1922 Churchill White Paper narrows the reading, the 1939 White Paper caps immigration near the point of catastrophe in Europe. Has diplomatic access to London and international Jewish organizations but cannot bind the mandatory power to any fixed textual meaning; each policy oscillation forces renewed lobbying from a shifted baseline.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_yishuv_leadership, payer,
    organized, generational, constrained, regional).

% The League of Nations body formally tasked with supervising mandate administration. Receives annual reports and can question British policy but has no enforcement power and no authority to overturn a mandatory power's interpretation. Its objections are recorded and occasionally embarrassing but never binding — it is present in form, absent in effect, which is precisely the gap this constraint exploits.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, permanent_mandates_commission, excluded,
    institutional, generational, analytical, global).

% Assess the documentary record of the mandate period after the fact, comparing the successive White Papers and land ordinances against the original instrument's text and against each community's contemporaneous expectations, without a stake in either outcome.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, historians_of_mandate_palestine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, vesting interpretive discretion in a single administering power could coordinate expectations by providing one authoritative reading of an ambiguous instrument rather than leaving two communities to fight over competing textual claims with no adjudicator at all.
% TRANSFER_FUNCTION: Moves strategic certainty and bargaining leverage away from both the Arab population and the Yishuv and toward the British administration: each side must continually re-litigate its position with London rather than relying on a fixed rule, which lets the administration extract compliance, quiescence, and diplomatic deference from both communities in exchange for temporary and revocable concessions.
% ABSENT_VOICES: Neither community had a forum to compel a binding, final interpretation; the Permanent Mandates Commission could hear grievances but not rule. Local Arab and Jewish institutions were consulted unevenly and often only after policy was substantially set in London, so their objections registered as commentary on a decision already made rather than input into its making.
% DISAPPEARANCE_RATIONALE: If the mandatory power's unreviewable interpretive discretion disappeared and were replaced by a fixed, externally enforceable reading of the mandate text, both communities could plan land purchase, immigration, and political organizing against a stable baseline rather than hedging against the next White Paper; the oscillating land regimes of 1920 and 1940 would not have both been possible under the same textual instrument.
% FOUNDING_PROBLEM: The mandate instrument's language on the 'Jewish national home' and the rights of 'existing non-Jewish communities' was drafted ambiguously to secure Zionist, Arab, and inter-allied assent simultaneously during the San Remo and League ratification process; someone had to be given authority to operationalize it since the text could not adjudicate itself.
% FOUNDING_PROBLEM_CORROBORATION: British officials at the time (e.g. the Churchill White Paper's own drafters) attested that discretion was a practical necessity given the text's internal tension. Independent League of Nations Permanent Mandates Commission members and later international-law scholars outside both the British administration and the two mandate communities have documented that the discretion was exercised inconsistently in ways that tracked British strategic interest (Arab revolt suppression, wartime Arab alliance needs, post-Holocaust immigration pressure) rather than a stable interpretive method, supporting the reading that the founding problem became a standing tool of administrative leverage rather than a one-time interpretive necessity.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the discretion does not extract material resources directly — it extracts something more structural: the capacity of both communities to rely on any fixed rule to plan against. Suppression is substantial (0.62) because the administration actively enforces its readings (through land ordinances, immigration quotas, and military responses to unrest) once issued, even though the readings themselves are unstable. Theater ratio is moderate (0.4) reflecting real administrative functions (actual governance, actual land registries) alongside a performative layer of 'neutral arbitration' that masks the asymmetric bargaining power built into unreviewable discretion. The rising suppression trajectory through the Arab Revolt (1936) and its partial relaxation by 1939 traces the tightening and loosening of enforcement machinery as the administration's strategic priorities shifted from suppressing Arab revolt to securing Arab wartime cooperation.
 *
 * DIRECTIONALITY LOGIC:
 *   The administration is the structural beneficiary: discretion is a resource it holds and deploys, never a cost it bears, and its exit options are arbitrage-grade — it can revise policy at will without needing either community's consent. Both Arab and Zionist populations are targets of the same mechanism from different angles: neither can convert political investment into a durable legal entitlement, because the administration retains sole authority to redefine what the mandate instrument means whenever the strategic balance shifts. This is why both communities are named as victims of the SAME constraint even though their substantive interests conflict — the meta-level extraction (uncertainty, lack of appeal) is symmetric across them even where the object-level dispute is zero-sum.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an ambiguous text needing SOME interpretive mechanism to be administrable at all — was arguably live in 1920 when the mandate was newly ratified and untested. By the mid-1930s, however, the discretion had become a standing tool for extracting quiescence and postponing resolution rather than a genuinely necessary interpretive stopgap; the corroboration from outside the administration (Permanent Mandates Commission objections, later scholarly consensus) supports reading founding_problem_status as contested rather than cleanly live or dead: the interpretive necessity did not disappear, but its exercise increasingly served administrative interest over resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_necessity_vs_instrumentalization,
    'Was unreviewable mandatory discretion a genuinely necessary feature of administering an irreducibly ambiguous instrument in 1920, or did it become primarily a tool for administrative leverage over both communities once the initial interpretive problem could have been resolved by other means (arbitration, League ruling, bilateral negotiation)?',
    'Compare the frequency and direction of policy reversals against documented triggers: reversals correlated with external legal/institutional pressure (genuine interpretive necessity) versus reversals correlated with British strategic interest alone (instrumentalized discretion). Archival record of Colonial Office internal deliberations would be the primary evidence.',
    'If discretion was principally instrumentalized rather than necessary, this reading is closer to a pure snare (near-zero coordination function); if genuinely load-bearing given the instrument''s ambiguity, it retains a defensible coordination component alongside the extraction, closer to a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_necessity_vs_instrumentalization, conceptual, 'Whether the discretion mechanism was interpretively necessary or became purely instrumental extraction.').

omega_variable(
    kernel_framing_which_layer_is_the_constraint,
    'Is the meta-level discretion (this reading) genuinely a distinct constraint from the substantive readings (jewish_national_home_primacy, dual_obligation_indigenous_rights), or is it simply the mechanism by which one substantive reading temporarily prevails — i.e. is ''discretion'' actually just the observable trace of whichever substantive reading is currently winning, rather than an independent structure?',
    'Examine whether the administration ever exercised discretion in ways that served NEITHER substantive reading fully (pure administrative self-interest, e.g. security considerations unrelated to either community''s claims) — if such cases exist, discretion is a genuinely independent layer; if every exercise of discretion maps cleanly onto favoring one substantive reading over the other, discretion may be epiphenomenal on the substantive contest.',
    'If discretion is epiphenomenal, this story should be merged into or subordinated to the substantive-reading stories rather than treated as an ε-invariant sibling; if independent, the three-way kernel decomposition is structurally sound as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_which_layer_is_the_constraint, conceptual, 'Whether the discretion-as-constraint framing is structurally independent of the two substantive readings or derivative of them.').

omega_variable(
    external_review_counterfactual,
    'Would binding external review (e.g. a Permanent Court of International Justice ruling with enforcement teeth) have actually produced a stable, fixed interpretation, or would the underlying substantive conflict between the two communities have made any fixed textual reading equally contested and equally in need of continual re-adjudication?',
    'Comparative study of other interwar mandates or minority-treaty regimes that had stronger external review mechanisms, assessing whether stability of interpretation actually reduced strategic uncertainty for affected populations in those cases.',
    'If external review would not have produced meaningfully more stability, the extraction attributed to ''unreviewable discretion'' specifically (versus the underlying textual ambiguity itself) is overstated, and part of the measured ε here belongs instead to the textual-ambiguity mountain, not to the discretion snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_review_counterfactual, empirical, 'Whether external review would have counterfactually reduced the strategic uncertainty attributed to unreviewable discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1922, 0.28).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1929, 0.35).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.38).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.42).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.4).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1922, 0.46).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1929, 0.52).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.6).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1922, 0.45).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1929, 0.58).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.7).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This story is the third member of the balfour_mandate_instruments kernel family. jewish_national_home_primacy and dual_obligation_indigenous_rights each author a substantive reading of the mandate text with a correspondingly different ε (each treats the OTHER side's claim as the extractive imposition). This story authors the meta-level claim that the mandatory power's unreviewable discretion to choose between those readings is itself the operative constraint, independent of which substantive reading currently prevails — its ε (0.58) reflects the harm of unappealable interpretive authority itself, not the content of any given interpretation. All three stories should be read together as a decomposition of the colloquial single label 'the Balfour Declaration / mandate for Palestine.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
