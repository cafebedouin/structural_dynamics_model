% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Mandate Instruments Read as Jewish National Home Primacy
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Balfour/Mandate
 *   kernel: the reading under which 'national home' is understood as
 *   requiring active demographic and territorial transformation toward Jewish
 *   sovereignty, with the Jewish Agency's quasi-governmental status (Article
 *   4), facilitated land transfer, and immigration policy all read as the
 *   mandate's operative content. Under this reading the arrangement functions
 *   as a high-epsilon tangled rope — genuine coordination for the immigrant
 *   population's settlement, riding on asymmetric extraction from the
 *   existing Arab population's land tenure and political standing. This is
 *   not a claim about which reading is correct; it is the classification this
 *   specific reading produces when its structural data (beneficiaries,
 *   victims, enforcement) is run through the engine. The sibling readings
 *   (dual_obligation_indigenous_rights; mandatory_interpretive_discretion)
 *   are separate constraints with their own epsilon and are not blended into
 *   this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.81).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.76).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.81).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Mandate Instruments Read as Jewish National Home Primacy").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '4ae9863f-889d-4e3c-a12a-6770b88083b7').
narrative_ontology:cs_kernel_codification('4ae9863f-889d-4e3c-a12a-6770b88083b7', fixed_text).
narrative_ontology:cs_authority_grounding('4ae9863f-889d-4e3c-a12a-6770b88083b7', extraction).
narrative_ontology:cs_interpretation_layer_present('4ae9863f-889d-4e3c-a12a-6770b88083b7').
narrative_ontology:cs_reading_relation('4ae9863f-889d-4e3c-a12a-6770b88083b7', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('4ae9863f-889d-4e3c-a12a-6770b88083b7', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('4ae9863f-889d-4e3c-a12a-6770b88083b7', foundational, national_home_requires_demographic_sovereignty).
narrative_ontology:cs_axiom_status(national_home_requires_demographic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4ae9863f-889d-4e3c-a12a-6770b88083b7', national_home_requires_demographic_sovereignty, conventional).
narrative_ontology:cs_axiom('4ae9863f-889d-4e3c-a12a-6770b88083b7', foundational, existing_population_rights_subordinate_to_national_home_clause).
narrative_ontology:cs_axiom_status(existing_population_rights_subordinate_to_national_home_clause, holdable).
narrative_ontology:cs_axiom_grounding('4ae9863f-889d-4e3c-a12a-6770b88083b7', existing_population_rights_subordinate_to_national_home_clause, conventional).
narrative_ontology:cs_reference_frame('4ae9863f-889d-4e3c-a12a-6770b88083b7', balfour_declaration_zionist_settlement_mandate).
narrative_ontology:cs_drift_state('4ae9863f-889d-4e3c-a12a-6770b88083b7', post_1939_white_paper_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ae9863f-889d-4e3c-a12a-6770b88083b7', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, national_home_as_proto_state_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish Agency and affiliated bodies are granted quasi-governmental status under Mandate Article 4, empowered to advise and cooperate with the administration on matters affecting the Jewish population, coordinate land purchase and settlement, and channel immigration. They administer institutions that function as a state-in-formation, drawing legal standing directly from the mandate text and shaping its implementation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter).

% Enter under immigration provisions read as facilitating demographic transformation toward Jewish sovereignty; gain access to land systematically transferred through Zionist-coordinated purchase mechanisms and to institutional infrastructure (labor federations, agricultural settlement bodies, municipal structures) built with quasi-state backing. Their arrival is the mandate's operative content under this reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrant_settlers, beneficiary,
    moderate, biographical, mobile, regional).

% Face systematic facilitation of land sales from absentee and indebted Arab owners to Jewish purchasers, administrative practices (land registration, tenancy law application, taxation) that favor transfer over retention, and displacement of tenant cultivators with no equivalent institutional apparatus representing their interests. Legal recourse runs through a mandatory administration structurally aligned with the transformation project.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    powerless, biographical, trapped, local).

% Denied the elected legislative council and proportional representation structures repeatedly proposed and repeatedly withdrawn once demographic ratios threatened Zionist objectives; excluded from an institutional counterpart to the Jewish Agency; their objections are recorded in commissions of inquiry but do not alter the operative reading of the mandate text.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, excluded,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer).

% Administers the mandate, issues immigration quotas and land transfer regulations, and enforces the reading of 'national home' as requiring active facilitation of Jewish demographic and territorial expansion, while periodically commissioning inquiries (Peel, Passfield) that document Arab dispossession without reversing the underlying facilitation apparatus.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_administration, agenda_setter,
    institutional, generational, constrained, national).

% Reviews annual mandatory reports and can question the administration's conduct but has no enforcement power over interpretation; treats the national-home clause as binding policy rather than one contested reading among several, lending international legal cover to the transformation project.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_permanent_mandates_commission, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the influx, settlement, and institutional self-organization of a specific immigrant population under a single legal-administrative umbrella, solving genuine problems of land registry, agricultural development, and municipal governance for that population.
% TRANSFER_FUNCTION: Moves land tenure, political representation capacity, and demographic weight from the existing Arab majority population to an incoming Jewish minority population, operating through immigration quotas, land transfer facilitation, and asymmetric grants of quasi-governmental standing.
% ABSENT_VOICES: Palestinian Arab political leadership repeatedly petitioned for representative institutions proportional to population and for restriction of land transfers; their objections appear in the historical record (Shaw Commission, Peel Commission testimony) but were not treated as co-equal claims on the mandate's meaning under this reading.
% DISAPPEARANCE_RATIONALE: If this reading of the mandate instruments were withdrawn overnight, immigration facilitation and systematic land-transfer administration would lose their legal warrant, the Jewish Agency's quasi-governmental standing would require renegotiation from scratch, and Arab political leadership would regain a plausible claim to proportional representative institutions — the demographic and territorial trajectory of Mandatory Palestine would be structurally altered, not merely relabeled.
% FOUNDING_PROBLEM: The Balfour Declaration and subsequent mandate text were built to reconcile a British wartime commitment to Zionist organizations with continued British control over Palestine's strategic position, while nominally preserving the civil and religious rights of the existing population.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and later Israeli state historiography attest the founding problem (establishing a Jewish national home) remained live and was substantially fulfilled by this reading's operation. Independent corroboration from outside the beneficiary set — the League of Nations' own Permanent Mandates Commission correspondence, British commission-of-inquiry findings (Shaw 1930, Peel 1937), and Palestinian Arab leadership testimony — attests that the 'existing population's rights' half of the founding problem was treated as subordinate almost from the outset, supporting the contested-status designation rather than a settled one.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction rises steadily across the interval (0.35 to 0.81) tracking the accelerating scale of land transfer and immigration facilitation from the early mandate years to the late 1930s. Suppression rises even faster (0.30 to 0.76) because maintaining this reading against a growing, increasingly organized Arab resistance (culminating in the 1936-39 revolt) required escalating administrative and military enforcement — suppression here is the raw structural cost of holding the reading in place against contestation, not scaled by any other dimension. Theater ratio stays comparatively low and only modestly rising (0.15 to 0.32): the coordination function (settlement administration, land registry, agricultural development) was substantially real, not primarily performative, even as its extractive share grew.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish immigrant settlers are declared beneficiaries with mobile-to-arbitrage exit options: the arrangement's entire operative logic runs in their favor, and they hold institutional standing (Article 4) that lets them shape the very rules being applied to them. Palestinian Arab landholders and political leadership are declared victims with trapped/constrained exit: their land tenure and representative claims are the transfer's source, and the mandatory administration's enforcement apparatus is what prevents them from reversing or exiting the arrangement. The British mandatory administration sits as agenda_setter with only constrained exit of its own — it enforces this reading but is also bound by League of Nations oversight and metropolitan political pressure, producing genuine seat divergence between administrator-as-enforcer and administrator-as-constrained-agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling a wartime commitment with continued rights for the existing population) is authored as contested rather than flatly dead, because Zionist historiography treats the national-home objective as still live and substantially achieved through this reading's operation, while corroborating outside sources (commissions of inquiry, League correspondence, Arab leadership testimony) document that the population-rights half of the founding problem was subordinated almost from inception. This prevents mislabeling the arrangement as pure Mountain (natural, inevitable state-formation) or pure Rope (costless coordination): the coordination function for Jewish settlement was real, but it was built on and required continuous, escalating suppression of a directly displaced population — the tangled_rope classification holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_home_textual_determinacy,
    'Does the mandate text and its drafting history determinately support the proto-state / demographic-transformation reading of ''national home,'' or is that reading itself a contested interpretive choice layered onto genuinely ambiguous text?',
    'Comparative analysis of the mandate drafting record (Balfour Declaration correspondence, San Remo conference minutes, League of Nations Council debates) against the competing dual_obligation_indigenous_rights reading''s textual claims; assessment of whether contemporaneous British officials themselves treated the reading as settled or as one live option.',
    'If the text is genuinely determinate toward this reading, the high epsilon reflects the mandate''s actual operative design. If the text is ambiguous and this reading was one interpretive choice among defensible alternatives, the extraction measured here is partly an artifact of administrative choice rather than textual necessity — which is exactly the terrain the mandatory_interpretive_discretion sibling constraint is built to capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_home_textual_determinacy, conceptual, 'Whether the proto-state reading is textually compelled or one interpretive choice among live alternatives.').

omega_variable(
    quasi_state_status_naturalization,
    'Is the Jewish Agency''s quasi-governmental status under Article 4 a natural consequence of any workable national-home policy, or a specific extractive design choice that could have been structured with an equivalent Arab institutional counterpart?',
    'Comparative mandate analysis: examine whether other mandates (Iraq, Transjordan, Syria) granted equivalent quasi-governmental standing to minority or immigrant populations, and whether proposals for an Arab Agency or legislative council with comparable standing were seriously considered and rejected.',
    'If comparable arrangements were never seriously offered to the Arab population despite proposals, the asymmetry is a designed extraction feature, not incidental to coordination — reinforcing the tangled_rope claim. If offered and structurally infeasible for independent reasons, part of the measured suppression may reflect circumstances beyond this reading''s design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quasi_state_status_naturalization, empirical, 'Whether the asymmetric institutional grant was a designed feature or an incidental byproduct.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'This is not authored as a mountain (no emerges_naturally claim), but is there a risk that historiographic framing treats the demographic-transformation trajectory as an inevitable unfolding of the mandate''s ''true meaning'' rather than a constructed, actively-enforced administrative choice?',
    'Track whether contemporaneous British administrators, League commissioners, or Zionist officials themselves described the trajectory as inevitable/natural versus as a policy requiring continuous active defense (immigration quota fights, land transfer ordinances, suppression of the 1936-39 revolt).',
    'The escalating suppression_requirement measurements (0.30 to 0.76) already argue against naturalness — a genuinely natural unfolding would not require rising enforcement. This omega flags the interpretive risk explicitly so the tangled_rope classification is not later reframed as a mountain by selective quotation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Guard against retroactive naturalization of an actively-enforced administrative trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1917, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1917, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1922, 0.18).
narrative_ontology:measurement(balf_tr_t1926, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1926, 0.21).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1930, 0.26).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1935, 0.29).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.32).

% Extraction over time
narrative_ontology:measurement(balf_be_t1917, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1922, 0.48).
narrative_ontology:measurement(balf_be_t1926, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1926, 0.58).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1930, 0.67).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1935, 0.76).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1917, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1922, 0.42).
narrative_ontology:measurement(balf_su_t1926, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1926, 0.5).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1930, 0.61).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1935, 0.7).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'the Balfour Declaration / Mandate for Palestine' per the ε-invariance principle. jewish_national_home_primacy (this file) reads the mandate as substantively directing Jewish demographic/territorial transformation — high epsilon tangled_rope, beneficiaries Zionist institutions and Jewish settlers, victims Arab landholders and political leadership. dual_obligation_indigenous_rights reads the same text as imposing an equal-or-superior protective obligation toward the existing population, substantially inverting the beneficiary/victim structure. mandatory_interpretive_discretion relocates the operative constraint to the British administration's unreviewed authority to choose between readings, making the discretion itself — rather than either substantive outcome — the object of classification. Each carries its own stable epsilon; none is a measurement of the others under a different observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
