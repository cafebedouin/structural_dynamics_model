% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Mandate Instruments Read as Jewish National Home Primacy
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   Between 1920 and 1939, the Mandate for Palestine was administered in a
 *   manner that, on this reading, treated the 'national home' clause as
 *   authorizing systematic facilitation of Jewish immigration, land
 *   acquisition, and institutional self-organization through the Jewish
 *   Agency's Article 4 status, while withholding proportional representative
 *   government from the Arab majority on the express ground that it would be
 *   used to block that facilitation. The 1929 disturbances and the 1936–39
 *   Arab Revolt mark points where resistance to the demographic and
 *   territorial trajectory intensified, followed by intensified enforcement
 *   (suppression_requirement rising through the period) rather than a change
 *   in interpretive course until the 1939 White Paper began to constrain it
 *   near the end of the interval.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.81).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.74).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.81).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Mandate Instruments Read as Jewish National Home Primacy").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, 'ac181d32-cf78-4116-9d07-4f80490d61ff').
narrative_ontology:cs_kernel_codification('ac181d32-cf78-4116-9d07-4f80490d61ff', fixed_text).
narrative_ontology:cs_authority_grounding('ac181d32-cf78-4116-9d07-4f80490d61ff', extraction).
narrative_ontology:cs_interpretation_layer_present('ac181d32-cf78-4116-9d07-4f80490d61ff').
narrative_ontology:cs_reading_relation('ac181d32-cf78-4116-9d07-4f80490d61ff', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('ac181d32-cf78-4116-9d07-4f80490d61ff', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('ac181d32-cf78-4116-9d07-4f80490d61ff', foundational, national_home_requires_sovereign_infrastructure).
narrative_ontology:cs_axiom_status(national_home_requires_sovereign_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('ac181d32-cf78-4116-9d07-4f80490d61ff', national_home_requires_sovereign_infrastructure, conventional).
narrative_ontology:cs_axiom('ac181d32-cf78-4116-9d07-4f80490d61ff', secondary, demographic_transformation_is_legitimate_mandate_object).
narrative_ontology:cs_axiom_status(demographic_transformation_is_legitimate_mandate_object, overridden).
narrative_ontology:cs_axiom_grounding('ac181d32-cf78-4116-9d07-4f80490d61ff', demographic_transformation_is_legitimate_mandate_object, empirically_contingent).
narrative_ontology:cs_reference_frame('ac181d32-cf78-4116-9d07-4f80490d61ff', balfour_declaration_national_home_commitment).
narrative_ontology:cs_drift_state('ac181d32-cf78-4116-9d07-4f80490d61ff', id_1939_white_paper_reversal, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ac181d32-cf78-4116-9d07-4f80490d61ff', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish Agency and affiliated bodies are recognized under Article 4 of the Mandate as a quasi-governmental partner to the administration, consulted on immigration policy, land settlement, and public works. They coordinate capital inflows, organize systematic land purchase from absentee and indebted Arab landowners, and lobby the mandatory power directly for favorable immigration quotas. Their institutional status under the Mandate text is itself the mechanism that converts a stated 'national home' into functional proto-state infrastructure.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter).

% Enter under an immigration framework structurally weighted to expand the Jewish demographic share, gain access to land parcels the Zionist institutions systematically acquire, and settle into an emerging parallel economy and civil administration. Their arrival and settlement are the demographic transformation the Mandate's 'national home' language is read, on this reading, to require.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers, beneficiary,
    moderate, biographical, mobile, national).

% Face systematic pressure toward land transfer through debt, absentee-landlord sales conducted over their heads, and a legal and administrative apparatus that facilitates rather than restricts land movement toward Jewish ownership. Many are tenant cultivators (fellahin) with no formal title, and lose access to land they have worked for generations when it is sold by a distant owner; they have no institutional analogue to the Jewish Agency through which to contest terms.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, generational, trapped, local).

% Repeatedly petitions the mandatory administration for representative self-government proportional to the Arab majority population, and is repeatedly refused on the ground that any elected body would use its majority to foreclose the national home commitment. Their political institutions remain informal and unrecognized while the Jewish Agency's parallel structure is formalized in the Mandate text itself — a structural downgrading of political standing relative to demographic weight.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    moderate, generational, constrained, national).

% Administers immigration certificates, land transfer registration, and the recognition of the Jewish Agency's consultative role. On this reading, the administration interprets its own textual mandate as requiring active facilitation of Jewish demographic and institutional growth, and enforces this interpretation against Arab objection through police power, certificate quotas, and refusal of representative government. It bears enforcement costs and periodic unrest but retains discretion over how hard to push the transformation at any given moment.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_administration, agenda_setter,
    institutional, biographical, constrained, national).

% Receives annual reports from the mandatory power and hears petitions from both communities, but has no independent enforcement capacity and defers heavily to the administering power's own account of compliance. Functions as a nominal check that in practice ratifies whichever reading the mandatory power adopts.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_mandate_commission, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital, migration flows, and institutional development among Zionist organizations and the mandatory administration to build the demographic and institutional infrastructure of a future Jewish polity within a single legal instrument, avoiding the coordination failure of dispersed, uncoordinated settlement.
% TRANSFER_FUNCTION: Moves land tenure, political standing, and demographic weight from the existing Arab population and its informal institutions to Jewish immigrant settlers and the formally recognized Jewish Agency, using the Mandate's textual ambiguity and the administration's enforcement power as the transfer mechanism.
% ABSENT_VOICES: Palestinian Arab tenant cultivators displaced by absentee-owner land sales have no seat at the drafting table and no institutional channel comparable to the Jewish Agency; their objections surface only as petitions and unrest, which this reading treats as disorder to be managed rather than evidence bearing on the reading's own legitimacy.
% DISAPPEARANCE_RATIONALE: If this reading of the Mandate text were abandoned overnight in favor of a strict trusteeship reading, immigration facilitation and land-transfer facilitation would need independent justification, the Jewish Agency's quasi-governmental status would lose its textual anchor, and the demographic trajectory toward Jewish sovereignty would slow or require an entirely different legal basis — the entire institutional apparatus built on this reading depends on it remaining the operative interpretation.
% FOUNDING_PROBLEM: The Balfour Declaration and subsequent Mandate text needed to be operationalized into administrative practice: what did 'establishing in Palestine a national home for the Jewish people' actually require the mandatory power to do, day to day, about immigration certificates, land sales, and political representation?
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutional leadership attests this reading correctly operationalizes the Balfour commitment and Article 4. Arab political leadership and, later, the 1930 Hope Simpson Report and 1937 Peel Commission findings — produced by British-appointed but formally independent inquiry — corroborate from outside the beneficiary set that land transfer and immigration practice under this reading was producing landlessness and political displacement inconsistent with the Mandate's stated protections for the existing population, i.e., that the transformation this reading directs was contested even within the administering power's own investigative apparatus.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.55→0.81) tracking the accumulation of land transfers and immigration under increasingly formalized institutional facilitation. Suppression tracks enforcement against Arab resistance — petitions, then the 1929 and 1936 uprisings, met with increasing administrative and police response, which is why suppression_requirement rises faster than theater_ratio: this is substantive coercive enforcement, not performance. Theater_ratio stays low-moderate because the coordination function (organizing capital and settlement) is genuinely operative throughout, not merely symbolic — the tangled_rope classification requires exactly this combination of real coordination function plus asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the Zionist institutional seat, the arrangement is coordination — building the infrastructure a national home requires, using the legal instrument as intended. From the Palestinian Arab landholder and political-leadership seats, the same instrument is enforced demographic and territorial transfer with no proportional political remedy. The mandatory administration occupies neither seat cleanly: it enforces the transformation while absorbing the costs of resulting unrest, which is why its own investigative commissions (Hope Simpson, Peel) increasingly corroborated the victim-seat account even as day-to-day administration continued facilitating the beneficiary-seat trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish immigrant settlers sit near the beneficiary end: the Jewish Agency collects formal institutional status and coordinates capital and settlement to its own design; settlers gain land access and demographic weight structurally facilitated for them. Palestinian Arab landholders sit near the full-target end — trapped exit, land transferred over their heads via absentee sales they cannot contest, no comparable institutional recourse. Palestinian Arab political leadership is a target with somewhat more mobility (moderate power, constrained exit) but is structurally denied the representative-government remedy that would let it contest the trajectory through ordinary political means — that denial is itself part of the extraction, not incidental to it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being mislabeled as either pure coordination (which would erase the documented land dispossession and political disenfranchisement) or pure extraction (which would erase the real coordination problem the Jewish Agency's institutional recognition solved for organizing capital and settlement). Both must be true simultaneously for tangled_rope to fire: a genuine coordination function (Article 4 institutional recognition solving a real organizational problem) coexisting with asymmetric extraction (systematic land transfer and political downgrading) sustained by active enforcement (immigration/land administration plus police response to resistance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_home_textual_indeterminacy,
    'Does the phrase ''national home for the Jewish people'' in the Balfour Declaration and Mandate preamble textually require demographic transformation toward Jewish political sovereignty, or is that reading an interpretive choice made by the mandatory power and Zionist institutions rather than a requirement of the text itself?',
    'Comparative textual and drafting-history analysis of the Balfour Declaration''s negotiation record, the Mandate''s Article 6 (facilitating immigration) versus Article 2 (safeguarding civil and religious rights of all inhabitants), and contemporaneous statements by the drafters (Balfour, Curzon, the Zionist Organization) about intended scope.',
    'If the text is genuinely indeterminate between this reading and the dual_obligation reading, then this constraint''s high ε reflects a chosen interpretation rather than a textually compelled one, strengthening the case that mandatory_interpretive_discretion is the deeper operative constraint beneath both substantive readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_home_textual_indeterminacy, conceptual, 'Whether Mandate text textually compels this reading or merely permits it among competing interpretations.').

omega_variable(
    land_transfer_causal_mechanism_specificity,
    'To what extent was Arab land dispossession in this period caused by Mandate-facilitated Zionist institutional purchasing versus by pre-existing Ottoman-era debt structures, absentee landlordism, and agricultural market pressures that predate and are independent of the Mandate?',
    'Land registry and title-transfer records cross-referenced against Jewish National Fund and Palestine Land Development Company acquisition records, compared against Ottoman-era tenure and debt patterns in the same districts before 1917.',
    'If dispossession was substantially pre-existing and Mandate facilitation only accelerated an already-operating process, ε should be somewhat lower than authored; if Mandate-era facilitation (immigration-funded capital inflows enabling purchase at scale) was the dominant causal driver, the authored high ε is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_transfer_causal_mechanism_specificity, empirical, 'Whether Mandate facilitation was the primary or an accelerating cause of land transfer.').

omega_variable(
    sibling_reading_framing_choice,
    'This story treats jewish_national_home_primacy as the operative reading throughout 1920-1939; the mandatory_interpretive_discretion reading would instead treat the administration''s unreviewed discretion to choose between readings, moment to moment, as the true operative constraint, with this reading being merely the discretion''s output in most (not all) periods.',
    'Track administrative decision points (1922 White Paper, 1930 Passfield White Paper, 1939 White Paper) to determine whether the operative reading actually shifted across the interval or remained constant — a shifting pattern would support the discretion-as-constraint framing over a single fixed substantive reading.',
    'If the operative reading shifted materially within the interval (which the 1939 White Paper''s tightening suggests), this single-reading story may be authoring one reading''s ε over a period when the actually-operative constraint was discretion switching between readings — supporting the case that mandatory_interpretive_discretion, not this reading, is the more temporally stable constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Whether the interval is better modeled as this reading holding constant, or as interpretive discretion oscillating between readings with this one as the dominant but not exclusive output.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(balf_tr_t1923, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1923, 0.18).
narrative_ontology:measurement(balf_tr_t1926, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1926, 0.2).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1929, 0.22).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1933, 0.25).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1936, 0.27).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.28).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(balf_be_t1923, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement(balf_be_t1926, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1926, 0.64).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1929, 0.7).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1933, 0.77).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1936, 0.8).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement(balf_su_t1923, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1923, 0.5).
narrative_ontology:measurement(balf_su_t1926, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1926, 0.55).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1929, 0.63).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1933, 0.7).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1936, 0.74).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the balfour_mandate_instruments kernel. dual_obligation_indigenous_rights authors the same text as imposing superior/equal obligation to protect existing Arab rights, with a correspondingly different (much lower or inverted) beneficiary/victim structure. mandatory_interpretive_discretion authors the deeper claim that the administration's unreviewed discretion to choose between readings is itself the operative constraint, prior to either substantive reading. All three share the same underlying Mandate text and interval but are structurally distinct constraints with distinct ε values, per the ε-invariance principle — they are not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
