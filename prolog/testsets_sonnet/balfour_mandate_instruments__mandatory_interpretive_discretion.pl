% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Power's Unreviewable Interpretive Discretion over the Palestine Mandate
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This story isolates the interpretive-discretion structure of the
 *   Palestine Mandate as its own constraint, distinct from either community's
 *   substantive reading of what the Mandate promises. The 1922 Churchill
 *   White Paper, the 1930 Passfield White Paper, the 1939 MacDonald White
 *   Paper, and the land transfer regulations of 1920 and 1940 each constitute
 *   a distinct administrative act reinterpreting the same founding text
 *   without any binding external check. Neither the Arab population nor the
 *   Zionist leadership can obtain a fixed, appealable ruling on what the
 *   instrument requires; each reinterpretation resets the negotiating
 *   baseline. This is deliberately NOT the same constraint as
 *   jewish_national_home_primacy or dual_obligation_indigenous_rights (the
 *   two substantive readings) — this story's ε tracks the cost of
 *   unreviewable discretion itself, which is present regardless of which
 *   substantive reading one thinks is textually correct.
 *
 * KEY AGENTS:
 *   - british_colonial_administrators: primary beneficiary/agenda-setter (institutional/arbitrage) — retains policy flexibility and strategic position
 *   - arab_palestinian_population: primary target (moderate/trapped) — cannot bank negotiated gains, unrest resets baseline
 *   - zionist_yishuv_leadership: primary target (moderate/constrained) — institutional investment stranded by reversal
 *   - league_of_nations_permanent_mandates_commission: excluded/theatrical oversight (organized/constrained) — advisory only
 *   - future_international_legal_scholarship: analytical observer (analytical/analytical) — documents pattern retrospectively
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
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Power's Unreviewable Interpretive Discretion over the Palestine Mandate").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '0257e89f-fc22-4f5b-9f79-d4fd8a8214d5').
narrative_ontology:cs_kernel_codification('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', formalized).
narrative_ontology:cs_authority_grounding('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', extraction).
narrative_ontology:cs_interpretation_layer_present('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5').
narrative_ontology:cs_reading_relation('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', foundational, mandatory_power_sole_adjudicator_without_appeal).
narrative_ontology:cs_axiom_status(mandatory_power_sole_adjudicator_without_appeal, holdable).
narrative_ontology:cs_axiom_grounding('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', mandatory_power_sole_adjudicator_without_appeal, conventional).
narrative_ontology:cs_axiom('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', secondary, textual_ambiguity_is_irreducible_and_requires_administrative_resolution).
narrative_ontology:cs_axiom_status(textual_ambiguity_is_irreducible_and_requires_administrative_resolution, overridden).
narrative_ontology:cs_axiom_grounding('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', textual_ambiguity_is_irreducible_and_requires_administrative_resolution, instrumental).
narrative_ontology:cs_reference_frame('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', wartime_diplomatic_ambiguity_compromise).
narrative_ontology:cs_drift_state('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', pre_war_1939_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0257e89f-fc22-4f5b-9f79-d4fd8a8214d5', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_imperial_strategic_interests).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_population).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_yishuv_leadership).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, mandatory_power_sole_adjudicator_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Mandate under League of Nations cover, issue White Papers (1922, 1930, 1939) and land-transfer regulations (1920, 1940) that each re-read the founding instrument's ambiguous 'national home' and 'civil and religious rights' language differently, and face no binding external tribunal that can overturn their interpretation. Each reinterpretation preserves administrative flexibility and the ability to play communities against one another to maintain order and imperial strategic position (Suez route, oil transit, regional alliances).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, beneficiary).

% The abstract strategic position (canal security, regional basing, wartime Arab alliance management) that the administrators' interpretive latitude is exercised to protect; it collects no rents directly but every discretionary policy shift is justified by reference to it.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_imperial_strategic_interests, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(balfour_mandate_instruments__mandatory_interpretive_discretion, british_imperial_strategic_interests).

% Petition, revolt (1936-39), and negotiate against a moving textual target: the 1922 White Paper narrows the national home commitment, the 1930 Passfield paper checks land transfer, but each concession is later reversed or diluted by subsequent administrative practice. They cannot obtain a fixed ruling on what the Mandate actually requires and each round of unrest resets the baseline the British negotiate from, so gains are never banked.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_population, payer,
    moderate, biographical, trapped, regional).

% Builds institutions (Jewish Agency, land purchase apparatus, immigration infrastructure) under a mandate text it reads as promising a national home, but faces immigration quotas and land regulation that shift by administrative fiat (1939 White Paper reversing prior facilitation) with no textual anchor it can invoke against London. Investment and demographic planning made under one reading are stranded when the reading changes.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_yishuv_leadership, payer,
    moderate, biographical, constrained, regional).

% Formally reviews annual British reports and can question policy, but has no power to compel a different interpretation or overturn an administrative act; its oversight is advisory and the mandatory power routinely proceeds regardless of Commission concerns. Functions as the theatrical layer of external review without binding force.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_permanent_mandates_commission, excluded,
    organized, generational, constrained, continental).

% Analyzes the Mandate instruments retrospectively, comparing the textual commitments against administrative practice across the interwar period, documenting the pattern of interpretive oscillation and its consequences for both communities without power to alter the historical record.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, future_international_legal_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, vesting interpretive authority in a single administering power solves the problem of a genuinely ambiguous founding text (the Balfour Declaration's language folded into the Mandate) needing SOME mechanism to produce administrable policy rather than perpetual textual paralysis.
% TRANSFER_FUNCTION: Moves the cost of textual ambiguity from the British administration (which retains full policy flexibility and can always claim good-faith interpretation) onto both Arab and Jewish communities, who must continually renegotiate their position from whatever baseline the latest reinterpretation establishes, forfeiting the ability to rely on prior commitments.
% ABSENT_VOICES: Neither Arab nor Jewish representative bodies had a vote in Mandate design or a binding say in its interpretation; the League of Nations Permanent Mandates Commission was structurally present but toothless. Later international legal scholarship documents the pattern but cannot retroactively bind the historical administrators.
% DISAPPEARANCE_RATIONALE: If unreviewable interpretive discretion had not existed — if a binding external tribunal or a fixed textual reading had governed from 1920 — both communities' strategic calculations, investment timing (Jewish land purchase and immigration planning; Arab political organizing), and diplomatic posture toward London would have been fundamentally different, since neither would have needed to hedge against sudden policy reversal.
% FOUNDING_PROBLEM: The Balfour Declaration's language ('national home for the Jewish people' alongside protection of 'civil and religious rights' of 'existing non-Jewish communities') was drafted ambiguously to secure wartime diplomatic support from multiple audiences simultaneously; once incorporated into the binding Mandate text, someone had to adjudicate what it actually required in practice.
% FOUNDING_PROBLEM_CORROBORATION: Post-Mandate international legal scholarship (including League of Nations Permanent Mandates Commission internal correspondence and post-1948 historical analysis) attests that the original ambiguity-management problem was resolved or superseded well before 1939, and that continued discretionary reinterpretation after that point served administrative and strategic convenience rather than any live need to manage the founding text's ambiguity; this corroboration comes from Commission records and later scholarship outside both the British administration and the two mandate communities.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58 by 1939) is moderate rather than extreme because the discretion also performs a genuine coordination function early on (1920-22): SOME interpretive mechanism was needed to move from an ambiguous wartime declaration to administrable policy. But the metric rises across the interval because each successive White Paper increasingly serves administrative and imperial-strategic convenience rather than good-faith textual clarification, consistent with the founding_problem_status of 'dead' by the late 1930s. Suppression (0.62) reflects the active administrative and, after 1936, military enforcement required to make each reinterpretation stick against organized resistance from both communities. Theater ratio (0.4) captures the growing gap between the stated commitment to League of Nations oversight and the toothlessness of that oversight in practice — the Permanent Mandates Commission process increasingly functions as legitimating theater around decisions already made administratively.
 *
 * PERSPECTIVAL GAP:
 *   From the administrators' seat, discretion is prudent, necessary flexibility in an ambiguous and volatile situation — a rope solving a genuine text-interpretation problem. From either community's seat, the same discretion computes as a snare: an unreviewable power that can always redefine the terms of the arrangement after the fact, with no textual or judicial recourse. The engine should register this divergence directly from the structural data (agenda_setter's arbitrage exit versus payers' trapped/constrained exit) rather than from any claim either side makes about its own good faith.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators sit at the full-beneficiary end: they set the interpretive rules, bear no cost from reversing prior commitments, and retain maximal policy flexibility (arbitrage exit — they can always reframe). Both communities sit near the full-target end despite opposite substantive interests, because the SHARED cost each bears is structural: neither can appeal to a fixed textual meaning or a binding external arbitrator. The Permanent Mandates Commission is excluded rather than a genuine check — it has voice but no binding authority, which is precisely the theatrical-oversight pattern the theater_ratio measurement tracks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing a genuinely ambiguous wartime diplomatic text) was arguably live and defensible in 1920-1922. By the mid-to-late 1930s, per corroborating League of Nations Commission records and later scholarship, the ambiguity-management function had been substantially resolved or overtaken by events, yet the discretionary mechanism was retained and intensified (1939 White Paper) because it continued to serve British strategic interest (managing Arab alliance needs on the eve of war) rather than any live interpretive necessity. This is the mismatch the R5 fields are built to surface: founding_problem_status = dead, disappearance_verdict = world_rearranges — a live signal of capture rather than genuine ongoing coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_necessary_or_pretextual,
    'Was British interpretive discretion a genuinely necessary response to irreducible textual ambiguity in the founding instrument, or was the ambiguity itself cultivated and then exploited as a pretext for policy flexibility serving imperial strategic interest?',
    'Comparative analysis of British Foreign Office and Colonial Office internal correspondence around each White Paper''s drafting, checking whether internal deliberation was driven by genuine legal uncertainty or by strategic calculation stated openly among administrators.',
    'If genuinely necessitated by textual ambiguity with good-faith attempts at consistent interpretation, this reading is closer to a scaffold (a temporary interpretive mechanism pending clearer international law norms) that degraded into extraction over time. If pretextual from early on, this reading is closer to a pure snare from inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_necessary_or_pretextual, empirical, 'Whether the interpretive discretion mechanism was genuinely necessitated or pretextual.').

omega_variable(
    commission_oversight_theatrical_or_substantive,
    'Did the League of Nations Permanent Mandates Commission ever meaningfully constrain British interpretive choices, or was its review process purely theatrical/legitimating throughout the Mandate period?',
    'Historical case study of instances where Commission objections led to policy modification versus instances where British administrators proceeded despite Commission concerns; quantify the rate of substantive versus cosmetic response.',
    'If the Commission occasionally bound British policy, the theater_ratio should be revised downward and some genuine external-review function credited; if never, the theater_ratio and suppression figures understate the completeness of unreviewability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commission_oversight_theatrical_or_substantive, empirical, 'Whether League of Nations oversight was ever substantively binding.').

omega_variable(
    kernel_framing_discretion_vs_substance,
    'Is the interpretive-discretion structure genuinely separable from the substantive readings (jewish_national_home_primacy, dual_obligation_indigenous_rights) as an independent constraint, or does authoring it separately obscure that the discretion was always exercised IN SERVICE of one substantive reading over the other at each juncture?',
    'Track whether each discretionary reinterpretation (1922, 1930, 1939 White Papers) consistently favored one community''s substantive reading over time, which would suggest the ''discretion'' framing undercounts a directional bias better captured by the sibling constraints.',
    'If discretion is shown to systematically favor one reading over time rather than genuinely oscillating, this story''s claim to be a distinct, symmetric-victim constraint weakens, and its ε and victim-symmetry should be revisited relative to the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_discretion_vs_substance, conceptual, 'Whether the interpretive-discretion framing is a genuinely distinct constraint or an artifact of decomposition.').


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
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.36).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.38).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.4).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1922, 0.42).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1929, 0.5).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.53).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1922, 0.45).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.56).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.6).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the balfour_mandate_instruments kernel. jewish_national_home_primacy and dual_obligation_indigenous_rights each claim the Mandate text itself resolves toward a particular substantive outcome; this story claims instead that the operative constraint is the unreviewable discretionary authority that sits ABOVE both substantive readings, permitting the mandatory power to invoke either reading opportunistically without being bound by either. The three stories share the same underlying instruments and time period but have structurally distinct ε profiles, beneficiary/victim sets, and classification logic, per the ε-invariance principle. This story's administrators are the beneficiary in all three readings' accounts, but here they benefit specifically from the ABSENCE of a fixed reading, whereas in the sibling readings the substantive content of the (contested) reading itself does the work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
