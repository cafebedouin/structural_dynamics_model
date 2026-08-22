% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: British Mandatory Discretion to Adjudicate Competing Readings of the Mandate Instrument
 *   domain: International Law / Colonial Administration / State Formation
 *
 * SUMMARY:
 *   This constraint isolates the mandatory power's retained authority to
 *   adjudicate between competing readings of the mandate instrument's
 *   ambiguous 'national home' language, without binding external review, as
 *   the operational structure governing British Palestine (1920-1948). This
 *   is distinct from the substantive readings themselves (the dual-obligation
 *   reading favoring Arab civil/political protection, and the
 *   national-home-primacy reading favoring Jewish sovereign development) —
 *   those are separate constraints in the same kernel family. What is claimed
 *   here is the meta-level fact that discretion itself, exercised repeatedly
 *   and reversibly across the 1922, 1930, and 1939 White Papers and
 *   successive land ordinances, functioned as the actual constraint both
 *   communities lived under, independent of which substantive reading
 *   happened to be operative at a given moment.
 *
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
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Discretion to Adjudicate Competing Readings of the Mandate Instrument").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "International Law / Colonial Administration / State Formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '78809f18-56da-4397-8873-37a8323e2149').
narrative_ontology:cs_kernel_codification('78809f18-56da-4397-8873-37a8323e2149', fixed_text).
narrative_ontology:cs_authority_grounding('78809f18-56da-4397-8873-37a8323e2149', extraction).
narrative_ontology:cs_interpretation_layer_present('78809f18-56da-4397-8873-37a8323e2149').
narrative_ontology:cs_reading_relation('78809f18-56da-4397-8873-37a8323e2149', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('78809f18-56da-4397-8873-37a8323e2149', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('78809f18-56da-4397-8873-37a8323e2149', foundational, administering_power_holds_final_unreviewable_interpretive_authority).
narrative_ontology:cs_axiom_status(administering_power_holds_final_unreviewable_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('78809f18-56da-4397-8873-37a8323e2149', administering_power_holds_final_unreviewable_interpretive_authority, conventional).
narrative_ontology:cs_axiom('78809f18-56da-4397-8873-37a8323e2149', secondary, textual_ambiguity_licenses_reversible_policy_rather_than_binding_settlement).
narrative_ontology:cs_axiom_status(textual_ambiguity_licenses_reversible_policy_rather_than_binding_settlement, holdable).
narrative_ontology:cs_axiom_grounding('78809f18-56da-4397-8873-37a8323e2149', textual_ambiguity_licenses_reversible_policy_rather_than_binding_settlement, instrumental).
narrative_ontology:cs_reference_frame('78809f18-56da-4397-8873-37a8323e2149', league_covenant_article_22_sacred_trust).
narrative_ontology:cs_drift_state('78809f18-56da-4397-8873-37a8323e2149', post_1939_white_paper, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('78809f18-56da-4397-8873-37a8323e2149', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandate_officials).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_population_of_palestine).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_settlement_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_settlement_institutions).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_mandate_system_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over the mandate text's ambiguous phrases ('national home,' 'civil and religious rights of existing non-Jewish communities') and issues White Papers (1922, 1930, 1939) and land transfer ordinances that shift the operative meaning of the mandate without amending the underlying instrument or submitting to binding external review. Each reissued interpretation resets the baseline both communities must negotiate from, preserving administrative flexibility and bargaining leverage over both parties simultaneously.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration, beneficiary).

% Career administrators (high commissioners, colonial secretaries) whose bureaucratic latitude and post-tenure prospects depend on being seen as managing (rather than resolving) the conflict; oscillating interpretation lets each official claim responsiveness to 'facts on the ground' while avoiding accountability for consistent principle, and they can rotate out before the consequences of any given reading mature.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandate_officials, beneficiary,
    institutional, biographical, mobile, national).

% Cannot obtain a fixed, appealable reading of the mandate's protections for 'existing non-Jewish communities'; each policy oscillation (1922 White Paper concession, 1930 Passfield restrictions reversed by the 1931 MacDonald letter, 1939 White Paper limiting immigration only after demographic transformation had already occurred) arrives too late or is itself later reversed, leaving land tenure and political status permanently provisional and denying any stable ground for organizing resistance or negotiation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_population_of_palestine, payer,
    powerless, biographical, trapped, regional).

% Built institutions (Jewish Agency, land purchase apparatus, immigration infrastructure) premised on the 1922 and earlier readings favoring national home construction, then faced abrupt reinterpretation in 1939 restricting immigration and land purchase precisely when external threat made the stakes existential; they gained real benefit from early readings but bore the cost of discretion's reversibility exactly when reversal was least survivable, with no external forum to compel consistency.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_settlement_institutions, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_settlement_institutions, beneficiary).

% The League of Nations body nominally supervising mandate administration; receives annual reports and can issue critical observations but has no binding authority to overturn a mandatory power's interpretation, no enforcement mechanism, and no standing to hear direct petitions from mandate populations, leaving it able to comment on discretion but never override it.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, permanent_mandates_commission, excluded,
    institutional, generational, analytical, continental).

% Assess the documentary record of shifting land regimes and White Papers after the fact, tracing how interpretive reversal itself (rather than any single reading) shaped irreversible demographic and political outcomes on the ground.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, historians_of_mandate_administration, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the mandatory power a single decision-making locus that can respond to shifting 'facts on the ground' (immigration waves, land purchases, unrest) without requiring textual amendment or multilateral renegotiation of the mandate instrument each time circumstances change.
% TRANSFER_FUNCTION: Moves the cost of interpretive uncertainty from the administering power (which retains flexibility and can revise course at will) onto both Arab and Zionist populations (who must repeatedly reorganize strategy, investment, and political claims around each newly announced reading, with no compensation for reliance on prior readings).
% ABSENT_VOICES: Neither the Arab population nor the Zionist institutions had standing to compel a binding, final interpretation from an external tribunal; the Permanent Mandates Commission could observe and criticize but not adjudicate. Both communities repeatedly petitioned London directly, but petition is not appeal.
% DISAPPEARANCE_RATIONALE: If mandatory discretion were removed and replaced by a fixed, externally adjudicated reading of the instrument's obligations, both communities could plan land acquisition, immigration, and political organizing around a stable baseline; British administrators would lose the divide-and-manage flexibility that let them extract cooperation from each community in turn by dangling favorable reinterpretation, and the entire path-dependent sequence of White Papers and land ordinances would have had no mechanism to occur.
% FOUNDING_PROBLEM: The mandate instrument's operative text ('national home for the Jewish people' alongside protection of 'civil and religious rights of existing non-Jewish communities') was drafted with irreconcilable ambiguity, requiring some administrative mechanism to translate contested language into day-to-day governance (immigration quotas, land transfer rules, representative institutions).
% FOUNDING_PROBLEM_CORROBORATION: British administrators themselves, in internal Colonial Office correspondence surrounding the 1930 Passfield White Paper and its 1931 reversal, acknowledged the ambiguity was being managed rather than resolved. Independent legal historians (e.g., analyses of the Permanent Mandates Commission's own minutes) corroborate from outside both benefiting parties that the Commission repeatedly flagged the instrument's internal contradiction as unresolved by any of the administering power's successive interpretations, rather than as a live drafting problem awaiting a correct answer.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58 by 1939) reflects the cumulative cost each community bore from having to repeatedly reorganize investment, land purchase, and political strategy around reinterpretations they could not appeal or lock in. Suppression (0.62) captures the structural absence of any forum — the Permanent Mandates Commission included — capable of binding the administering power to a fixed reading. Theater ratio (0.40) reflects that a substantial share of Mandate governance activity (commissions of inquiry, White Paper drafting, annual reports to Geneva) performed the appearance of principled adjudication while the underlying discretion remained unconstrained by any of it. The suppression_requirement series rises through the 1930s as Arab revolt (1936-39) and Jewish immigration pressure both intensified, requiring heavier administrative machinery (curfews, the Peel Commission, partition proposals) to hold the discretionary structure together against mounting resistance from both sides.
 *
 * DIRECTIONALITY LOGIC:
 *   British administrators and career officials sit at the beneficiary end: discretion is the asset that lets them manage rather than resolve the conflict, extracting continued (if resentful) engagement from both communities who have no better alternative than to keep petitioning London. Both the Arab population and Zionist institutions are targets despite their very different power levels and relationships to the mandate's substantive content — what unifies them as victims of THIS constraint (as opposed to the substantive readings) is that neither could convert prior investment or negotiated position into a stable, appealable baseline. The Zionist institutions had organized, resourced negotiating capacity (closer to symmetric on many disputes) but still bore the full cost of discretion's reversibility when the 1939 White Paper arrived; the Arab population, powerless and trapped, bore it more severely and had essentially no leverage to convert reversal-risk into compensation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (translating genuinely ambiguous mandate text into administrable policy) was real in 1920 but by the 1930s the arrangement had shifted from good-faith translation to a tool for extracting sequential cooperation from each community by dangling the possibility of a more favorable future reading. Classifying this as snare rather than tangled_rope is deliberate: while there is a real coordination problem (someone must adjudicate ambiguous text), the administering power's actual practice — repeatedly reversing course to manage unrest from whichever community was currently most threatening — shows the coordination story functioning primarily as cover for an extraction pattern (divide-and-manage) that persisted regardless of whether a stable reading was administratively available.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_as_kernel_ambiguity_or_deliberate_design,
    'Was the mandate instrument''s textual ambiguity an unavoidable drafting compromise later exploited administratively, or was open-ended interpretive discretion the intended design of the mandate system itself (a feature of the League of Nations mandate architecture generally, not specific to Palestine)?',
    'Comparative analysis of discretion exercised under other mandates (Syria/Lebanon, Iraq, South-West Africa) — if administering powers everywhere retained similarly unreviewable interpretive authority regardless of textual clarity, this points to systemic design; if Palestine''s ambiguity was unusually severe and discretion unusually exploited, this points to text-specific drift.',
    'If systemic design, this constraint generalizes to a class (mandatory discretion as such) rather than being specific to the Balfour instrument, changing how it should be linked in the broader mandate-system network. If text-specific, this constraint''s severity is tied more tightly to the Balfour text''s particular ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_as_kernel_ambiguity_or_deliberate_design, conceptual, 'Whether interpretive discretion was systemic to the mandate system or specific to this instrument''s ambiguity.').

omega_variable(
    sibling_reading_foreclosure_relationship,
    'Does the mandatory power''s exercise of unreviewable discretion (this reading) logically foreclose either the jewish_national_home_primacy or dual_obligation_indigenous_rights readings from being simultaneously true accounts of the instrument''s meaning, or do all three coexist as different levels of description?',
    'Examine whether the two substantive readings could both be held as accurate accounts of what the mandatory power was OBLIGATED to do, while this reading describes what the mandatory power was STRUCTURALLY ABLE to do regardless of obligation — a levels-of-description distinction rather than a logical contradiction.',
    'If levels-of-description, all three readings coexist without foreclosure (as currently declared). If the discretion reading is instead a meta-claim that the substantive obligations were never binding at all, it would more strongly undercut (though still not strictly foreclose) both substantive readings'' normative force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_relationship, conceptual, 'Whether the discretion reading operates at a different level than the substantive readings or partially undercuts their normative force.').

omega_variable(
    extraction_beneficiary_precision,
    'Is the beneficiary of interpretive discretion best identified as ''the British colonial administration'' as an institution, or as specific factional interests within it (Colonial Office vs. Foreign Office vs. on-the-ground High Commissioners) who used discretion for divergent and sometimes conflicting purposes?',
    'Archival analysis of internal Colonial Office correspondence and inter-departmental disputes over specific policy reversals (e.g., the 1930-31 Passfield-MacDonald reversal) to establish whether discretion served a unified institutional interest or was itself contested terrain between factions.',
    'If factional, the ''beneficiary'' declaration oversimplifies an internally contested administration, and effective extraction may be lower than modeled here because part of the apparent divide-and-rule pattern reflects genuine internal disagreement rather than coordinated strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_precision, empirical, 'Whether the administering power acted as a unified extractive beneficiary or as internally divided factions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1922, 0.3).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.35).
narrative_ontology:measurement(balf_tr_t1931, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1931, 0.38).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.38).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.4).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1922, 0.42).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.5).
narrative_ontology:measurement(balf_be_t1931, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1931, 0.48).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1922, 0.45).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(balf_su_t1931, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1931, 0.5).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.65).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This story is the meta-level reading of the balfour_mandate_instruments kernel: it describes the mandatory power's retained, unreviewable authority to choose and reverse between the two substantive readings (jewish_national_home_primacy and dual_obligation_indigenous_rights), rather than adjudicating which substantive reading is textually correct. Its epsilon (0.58, moderate snare) differs from both substantive readings because it measures the cost of reversibility and uncertainty itself, not the cost of either substantive policy direction. All three stories share the kernel_id balfour_mandate_instruments and are linked bidirectionally: this discretion reading influences the operating conditions under which each substantive reading can be asserted or reversed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
