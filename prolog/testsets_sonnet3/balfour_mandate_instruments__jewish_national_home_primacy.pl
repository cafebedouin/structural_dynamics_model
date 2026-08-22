% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Mandate for Palestine — Jewish National Home as Proto-State Primacy
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This story instantiates the jewish_national_home_primacy reading of the
 *   Balfour/Mandate kernel: the Mandate instruments are read as directing
 *   demographic and territorial transformation toward Jewish sovereignty,
 *   with 'national home' operationalized as proto-state building via Jewish
 *   Agency quasi-governmental status, facilitated land transfer, and
 *   demographically-calibrated immigration. This is NOT the only defensible
 *   reading of the same text — the dual_obligation_indigenous_rights reading
 *   holds the protective clauses as co-equal or superior obligations, and the
 *   mandatory_interpretive_discretion reading holds that the British
 *   administration's adjudicative authority is itself the operative
 *   constraint. Each reading is authored as its own constraint with its own
 *   epsilon; this file does not average across them.
 *
 * KEY AGENTS:
 *   - zionist_institutions: Primary beneficiary (organized/arbitrage) — gains quasi-governmental standing and coordinated settlement apparatus
 *   - jewish_immigrant_settlers: Beneficiary (moderate/mobile) — demographic instrument and direct beneficiary of land and immigration facilitation
 *   - palestinian_arab_landholders: Primary target (powerless/trapped) — bears land dispossession with no reciprocal institutional protection
 *   - palestinian_arab_political_leadership: Secondary target (moderate/constrained) — structurally denied institutional parity with the Jewish Agency
 *   - british_mandatory_administration: Agenda-setter (institutional/arbitrage) — administers and enforces the demographic-transformation reading
 *   - league_of_nations_mandate_system: Analytical observer (institutional/analytical) — nominal oversight with thin enforcement capacity
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
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Mandate for Palestine — Jewish National Home as Proto-State Primacy").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '44cb3665-a55d-4acb-b7c3-b1059334b615').
narrative_ontology:cs_kernel_codification('44cb3665-a55d-4acb-b7c3-b1059334b615', fixed_text).
narrative_ontology:cs_authority_grounding('44cb3665-a55d-4acb-b7c3-b1059334b615', extraction).
narrative_ontology:cs_interpretation_layer_present('44cb3665-a55d-4acb-b7c3-b1059334b615').
narrative_ontology:cs_reading_relation('44cb3665-a55d-4acb-b7c3-b1059334b615', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('44cb3665-a55d-4acb-b7c3-b1059334b615', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('44cb3665-a55d-4acb-b7c3-b1059334b615', foundational, national_home_entails_sovereign_trajectory).
narrative_ontology:cs_axiom_status(national_home_entails_sovereign_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('44cb3665-a55d-4acb-b7c3-b1059334b615', national_home_entails_sovereign_trajectory, conventional).
narrative_ontology:cs_axiom('44cb3665-a55d-4acb-b7c3-b1059334b615', secondary, demographic_transformation_is_legitimate_mandate_instrument).
narrative_ontology:cs_axiom_status(demographic_transformation_is_legitimate_mandate_instrument, holdable).
narrative_ontology:cs_axiom_grounding('44cb3665-a55d-4acb-b7c3-b1059334b615', demographic_transformation_is_legitimate_mandate_instrument, instrumental).
narrative_ontology:cs_reference_frame('44cb3665-a55d-4acb-b7c3-b1059334b615', balfour_declaration_1917_national_home_clause).
narrative_ontology:cs_drift_state('44cb3665-a55d-4acb-b7c3-b1059334b615', post_1948_partition_and_statehood, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('44cb3665-a55d-4acb-b7c3-b1059334b615', '').
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

% The Jewish Agency and affiliated bodies are granted quasi-governmental status under Mandate Article 4, consulted on immigration and land policy, and administer settlement, land purchase coordination, and institution-building with the Mandatory power's active cooperation. They draft the practical machinery — land registries, immigration facilitation, para-state infrastructure — that this reading treats as the Mandate's core purpose.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter).

% Enter under immigration quotas explicitly calibrated to increase Jewish demographic share, gain access to land facilitated by systematic transfer arrangements, and benefit from institutions (labor federations, settlement companies, municipal structures) built around eventual Jewish sovereignty. Individually variable circumstances, but collectively the intended demographic instrument of the arrangement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers, beneficiary,
    moderate, biographical, mobile, regional).

% Face land sale mechanisms structurally facilitated toward Jewish buyers, tenant eviction following transfers, and no reciprocal apparatus protecting continued tenure or use rights. Many are tenant cultivators with no formal title, exposed when absentee landlords sell; they have no equivalent institutional body representing their interests in land policy and cannot exit the territory or the transformation underway.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, biographical, trapped, regional).

% Denied a body equivalent in status to the Jewish Agency; repeated proposals for representative self-government are structured (via requirements they cannot accept without conceding the national-home framework) to be non-viable, leaving Arab political voice without an institutional lever comparable to Article 4 status. Can petition, protest, and negotiate but cannot compel institutional parity.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    moderate, generational, constrained, regional).

% Administers the Mandate, implements immigration and land policy, and — under this reading — treats 'national home' as directional toward Jewish sovereignty, providing the enforcement apparatus (police, land registry, immigration control) that makes the demographic and territorial transformation operational rather than aspirational.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_administration, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Supervises Mandate compliance in principle through periodic reporting, but exercises no independent enforcement power over the Mandatory's implementation choices; its oversight is formally present but structurally thin, allowing the Mandatory's reading of 'national home' to operate largely unchecked.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_mandate_system, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the practical machinery of building a Jewish national home: settlement financing, land purchase, immigration absorption, and proto-governmental institution-building are organized centrally through the Jewish Agency rather than left to uncoordinated individual settlement, which would be slower and more chaotic.
% TRANSFER_FUNCTION: Moves land tenure, demographic weight, and institutional standing from the existing Arab population and its political leadership toward Jewish immigrant settlers and Zionist institutions, using the Mandate's legal and administrative apparatus as the transfer mechanism.
% ABSENT_VOICES: Palestinian Arab tenant cultivators displaced by land transfers, and Arab political leadership that repeatedly petitioned for representative government proportional to population, are structurally outside the institutional channels (Article 4 status, Jewish Agency consultation rights) that this reading treats as the Mandate's operative core.
% DISAPPEARANCE_RATIONALE: If this reading's operative apparatus disappeared overnight — Jewish Agency quasi-governmental status, facilitated land transfer, demographically-calibrated immigration quotas — the trajectory toward a Jewish-majority proto-state would halt or reverse; land tenure patterns, demographic composition, and the balance of political institutions in Mandate Palestine would all diverge sharply from their actual historical path.
% FOUNDING_PROBLEM: The Balfour Declaration and subsequent Mandate text were framed to establish a 'national home for the Jewish people' in Palestine while simultaneously pledging that nothing would prejudice the civil and religious rights of existing non-Jewish communities — a problem of reconciling a settler-national project with an already-populated territory under international mandate.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutional histories and Mandatory administration reports of the period attest that 'national home' was operationally read as directional toward sovereignty (the reading this constraint documents). Independent corroboration from outside the beneficiary set includes League of Nations Permanent Mandates Commission correspondence questioning demographic-transformation pace, and contemporaneous British colonial officials (e.g. the 1930 Passfield White Paper and Peel Commission testimony) who documented Arab dispossession and political exclusion as a consequence of this operative reading — though British policy itself oscillated and never fully repudiated the reading documented here.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.52 to 0.81 across the interval, tracking the accelerating pace of land transfer and immigration after 1929 and especially after 1933 (increased Jewish immigration following events in Europe intensified the demographic transformation this reading treats as the Mandate's core function). Suppression rises correspondingly (0.40 to 0.74) as the administration's enforcement apparatus — land registry controls, immigration policing, and later direct counterinsurgency measures during the 1936-1939 Arab revolt — hardens to sustain the transformation against rising Arab resistance. Theater ratio stays comparatively low and stable (0.18 to 0.28): the coordination function (settlement financing, institution-building) is substantially real, not primarily performative, which is precisely why this reading computes as tangled_rope rather than pure snare — there is a genuine coordination structure riding alongside the asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (British administration) and the beneficiary seat (Zionist institutions), the arrangement reads as legitimate coordination executing an internationally sanctioned mandate. From the payer seats (Arab landholders and leadership), the identical structure reads as enforced demographic and territorial displacement lacking reciprocal protection. The engine computes this divergence from the declared power/exit/scope data; this reading does not adjudicate which seat is correct — it authors the structure as the primacy reading sees it.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish immigrant settlers are declared beneficiaries: the Jewish Agency's Article 4 status is a direct institutional gain, and immigration/land facilitation are direct transfers toward settlers, so directionality sits near the beneficiary end for both. Palestinian Arab landholders (trapped exit, powerless) sit near the full-target end — dispossession without institutional recourse or the ability to leave the territory to escape the transformation. Palestinian Arab political leadership (moderate power, constrained exit) experiences a milder but still asymmetric target position: political and diplomatic exit routes exist (petitions, delegations to London, boycott) but no institutional lever compels parity with the Jewish Agency's standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a live mismatch: this reading treats the 'national home' clause as still-live and directional (founding_problem_status: contested, with corroboration split between Zionist/administrative sources affirming the primacy reading and League of Nations Permanent Mandates Commission correspondence flagging the pace of transformation as inconsistent with the protective clauses). The disappearance_verdict of world_rearranges together with a contested founding-problem status is the diagnostic signature this reading is built to expose: an arrangement whose coordination function is real (settlement infrastructure) but whose distributive core is asymmetric enough that removing it would visibly rearrange land tenure, demography, and political institutions — the mark of tangled_rope rather than either pure rope or pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_home_textual_indeterminacy,
    'Does ''the establishment in Palestine of a national home for the Jewish people'' in the Balfour Declaration and Mandate preamble denote a proto-sovereign state-building mandate, or a more limited cultural/religious homeland compatible with continued Arab political majority?',
    'Comparative textual and drafting-history analysis (Balfour Declaration drafts, Mandate negotiation records, contemporaneous statements by drafters such as Lord Curzon and Chaim Weizmann) weighed against how the clause was actually operationalized in administrative practice.',
    'If the textual indeterminacy resolves toward the limited reading, this constraint''s claimed_type and beneficiary/victim structure would need re-grounding as an administrative overreach rather than a textually mandated primacy; if it resolves toward the proto-state reading, this constraint''s structure is the textually faithful one and the dual_obligation reading becomes the strained interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_home_textual_indeterminacy, conceptual, 'Whether the Mandate text itself supports proto-state primacy or a narrower homeland reading.').

omega_variable(
    administrative_practice_vs_textual_mandate,
    'Was the demographic-transformation trajectory (land facilitation, immigration quotas, Jewish Agency status) a faithful execution of the Mandate''s actual terms, or a discretionary administrative choice that could have been read otherwise under mandatory_interpretive_discretion?',
    'Comparative case study against other Mandate territories under the same League of Nations system where analogous ''national home''-style clauses were absent, isolating how much of the trajectory is attributable to textual mandate versus administrative discretion.',
    'If administrative discretion accounts for most of the trajectory, the mandatory_interpretive_discretion reading captures more of the true operative constraint than this reading credits, and this reading''s epsilon may overstate textual necessity relative to administrative choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_practice_vs_textual_mandate, empirical, 'Whether the transformation traces to the text or to discretionary administrative choices.').

omega_variable(
    coordination_extraction_separability,
    'Was the Jewish Agency''s coordination function (settlement financing, institution-building) structurally separable from the asymmetric land/immigration transfer, or were they necessarily bundled under the Mandate''s actual administrative design?',
    'Comparative analysis of settlement projects that proceeded through voluntary land purchase without administrative facilitation versus those relying on Mandate-enabled transfer mechanisms and quota policy.',
    'If separable, the coordination function could in principle have proceeded without the asymmetric extraction, sharpening the tangled_rope classification (real coordination riding on avoidable extraction); if inseparable, the extraction was intrinsic to the coordination mechanism as designed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the Jewish Agency''s coordination role could have functioned without the asymmetric land/immigration transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(balf_tr_t1923, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1923, 0.2).
narrative_ontology:measurement(balf_tr_t1926, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1926, 0.22).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1929, 0.24).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1933, 0.26).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1936, 0.27).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.28).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(balf_be_t1923, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1923, 0.58).
narrative_ontology:measurement(balf_be_t1926, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1926, 0.62).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1929, 0.68).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1933, 0.76).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1936, 0.79).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balf_su_t1923, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1923, 0.48).
narrative_ontology:measurement(balf_su_t1926, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1926, 0.53).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1929, 0.61).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1933, 0.68).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1936, 0.72).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language label 'the Balfour/Mandate national home clause,' per the ε-invariance principle: measuring the same textual kernel by different lights yields structurally distinct claims with different beneficiary/victim sets and different epsilon values. jewish_national_home_primacy (this file) authors epsilon from the primacy reading's own view of the standing arrangement (0.81 at interval end, tangled_rope). dual_obligation_indigenous_rights authors epsilon from a reading centering the protective clauses as co-equal obligations. mandatory_interpretive_discretion authors epsilon from a reading centering the British administration's unreviewed adjudicative authority as the operative constraint, independent of which substantive reading prevails. All three are linked via affects_constraints rather than merged into one averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
