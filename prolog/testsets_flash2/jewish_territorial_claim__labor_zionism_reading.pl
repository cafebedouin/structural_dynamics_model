% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__labor_zionism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionism: Jewish National Regeneration via 'Conquest of Labor' and Settlement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the Labor Zionist reading of the Jewish
 *   territorial claim in Palestine, emphasizing national regeneration through
 *   socialist transformation, 'conquest of labor' (exclusive Jewish
 *   employment), and incremental settlement to build facts on the ground. It
 *   is a reading of the broader 'jewish_territorial_claim' kernel. The
 *   constraint is classified as a Tangled Rope due to its genuine
 *   coordination function for Jewish settlers (building a national home,
 *   creating a new society) coupled with severe, actively enforced extraction
 *   from Palestinian Arab laborers and landowners through economic exclusion
 *   and land acquisition. The metrics reflect the increasing extractiveness
 *   and suppression as the project advanced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.92).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism: Jewish National Regeneration via 'Conquest of Labor' and Settlement").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '5e818d10-308f-4ba9-80d3-8c7890b8f8bc').
narrative_ontology:cs_kernel_codification('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', formalized).
narrative_ontology:cs_authority_grounding('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', lineage).
narrative_ontology:cs_interpretation_layer_present('5e818d10-308f-4ba9-80d3-8c7890b8f8bc').
narrative_ontology:cs_reading_relation('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', foundational, national_regeneration_through_labor).
narrative_ontology:cs_axiom_status(national_regeneration_through_labor, holdable).
narrative_ontology:cs_axiom_grounding('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', national_regeneration_through_labor, conventional).
narrative_ontology:cs_axiom('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', foundational, incremental_state_building).
narrative_ontology:cs_axiom_status(incremental_state_building, holdable).
narrative_ontology:cs_axiom_grounding('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', incremental_state_building, instrumental).
narrative_ontology:cs_reference_frame('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', socialist_pioneering_ethos).
narrative_ontology:cs_drift_state('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', post_1948_statehood, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5e818d10-308f-4ba9-80d3-8c7890b8f8bc', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from exclusive employment in Jewish enterprises and access to land acquired by Zionist institutions. Their identity is deeply intertwined with the project of 'conquest of labor' and settlement, making exit unthinkable.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, beneficiary,
    organized, generational, identity_locked, regional).

% Administer land acquisition, settlement, and economic development, actively promoting 'Hebrew labor' policies. They enforce the exclusion of Arab labor from Jewish-owned enterprises and manage the incremental state-building process.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_institutions, agenda_setter,
    institutional, generational, constrained, regional).

% Are systematically excluded from the growing Jewish economy, losing traditional employment opportunities and facing economic marginalization. Their options are limited by structural barriers and the expanding Jewish economic sphere.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers, payer,
    powerless, immediate, trapped, local).

% Experience land acquisition by Zionist institutions, often through legal but coercive means, leading to displacement and loss of agricultural livelihoods. Their ability to resist is constrained by legal frameworks and institutional power.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_landowners, payer,
    moderate, biographical, constrained, local).

% Oversee the territory, often balancing competing claims. While not directly enforcing 'Hebrew labor', their policies on land sales and immigration indirectly facilitate the Labor Zionist project. They observe and react to escalating tensions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, observer,
    institutional, biographical, analytical, national).

% Oppose the Zionist project in its entirety, viewing 'conquest of labor' and settlement as an existential threat. They are excluded from the decision-making processes of Zionist institutions and the British Mandate, resorting to resistance.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_nationalist_movements, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish immigration, land acquisition, and economic development to build a self-sufficient Jewish national home in Palestine, fostering a new 'Hebrew' identity rooted in manual labor.
% TRANSFER_FUNCTION: Transfers land, economic opportunities, and political control from Palestinian Arab society to Jewish settlers and Zionist institutions, through policies of exclusive 'Hebrew labor' and continuous settlement.
% ABSENT_VOICES: Palestinian Arab political and labor representatives are systematically excluded from the Zionist institutions that drive this constraint, and their objections to 'Hebrew labor' and land acquisition are largely ignored by the British Mandate authorities, who prioritize maintaining order over addressing underlying grievances.
% DISAPPEARANCE_RATIONALE: If the Labor Zionist project and its enforcement vanished, the economic and demographic landscape of Palestine would fundamentally shift. Jewish immigration would likely slow, land acquisition would cease, and Palestinian Arab laborers would regain access to a broader labor market. The entire trajectory of national development would be altered.
% FOUNDING_PROBLEM: The 'Jewish Question' in Europe (antisemitism, lack of national territory) and the perceived 'degeneration' of Jewish life in the diaspora, leading to a desire for national regeneration through productive labor and a return to the land.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and Jewish settlers attest that the founding problem of Jewish national insecurity and the need for self-determination remains live. Palestinian Arab leaders and international observers, however, contest this, arguing that the 'solution' to the Jewish Question has created a new problem of dispossession for Palestinians, and that the original problem's status is now secondary to the ongoing conflict.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__labor_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__labor_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the 'conquest of labor' policy systematically denied economic opportunities to the indigenous population, while land acquisition dispossessed them. Suppression is very high due to the active enforcement of 'Hebrew labor' policies by Zionist institutions and the increasing use of force to secure settlements against Arab resistance. Theater ratio is low, as the project was intensely functional and goal-oriented, with little performative maintenance. The claimed type 'rope' reflects the internal self-perception of the Labor Zionist movement as a constructive, coordinating force, while the metrics reveal its extractive and suppressive reality for the affected Palestinian population.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish settlers and Zionist institutions, this was a necessary and just project of national self-determination and regeneration (a Rope). From the perspective of Palestinian Arabs, it was a process of dispossession and economic strangulation (a Snare). The engine's classification will highlight this divergence by computing a Tangled Rope, reflecting the dual nature of coordination for one group and extraction for another, maintained by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers and Zionist institutions are clear beneficiaries, gaining land, employment, and national infrastructure. Palestinian Arab laborers and landowners are clear victims, losing livelihoods and land. The British Mandate authorities are observers whose policies indirectly facilitate the constraint, while Arab nationalist movements are excluded voices actively resisting the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_hebrew_labor,
    'Was the ''conquest of labor'' a natural and necessary stage of national development, or an artificial, enforced economic separation?',
    'Counterfactual analysis of alternative economic integration models, or historical comparison with other settler movements that did not enforce labor exclusivity.',
    'If artificial, the extractiveness and suppression metrics are fully attributable to policy choices; if natural, a portion might be considered an unavoidable cost of establishing a new economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_hebrew_labor, conceptual, 'Whether ''Hebrew labor'' was an organic development or a coercive policy.').

omega_variable(
    mandate_complicity_level,
    'To what extent did British Mandate policies actively enable or merely passively permit the Labor Zionist project''s extractive elements?',
    'Archival research into British policy directives, enforcement actions (or lack thereof), and responses to Palestinian grievances regarding land and labor.',
    'Higher complicity would shift some responsibility for suppression and extraction to the Mandate authorities, potentially reclassifying their role from ''observer'' to a more active ''agenda_setter'' or ''beneficiary'' of the constraint''s stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_complicity_level, empirical, 'Degree of British Mandate complicity in Labor Zionist extraction.').

omega_variable(
    identity_lock_vs_economic_necessity,
    'For Jewish settlers, was ''identity_locked'' exit truly a matter of ideological commitment, or was it increasingly reinforced by economic necessity within the segregated Jewish economy?',
    'Sociological studies of settler motivations and economic conditions over time, examining the interplay between ideological fervor and material incentives for remaining within the Labor Zionist framework.',
    'If economic necessity became dominant, the ''identity_locked'' classification would gain a stronger ''constrained'' component, indicating a more structural rather than purely volitional binding to the project.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_economic_necessity, empirical, 'Interplay of ideology and economics in settler identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.05).
narrative_ontology:measurement(jewi_tr_t1918, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1918, 0.08).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.1).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement(jewi_be_t1918, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1918, 0.7).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.78).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1936, 0.83).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.5).
narrative_ontology:measurement(jewi_su_t1918, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1918, 0.65).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.75).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1936, 0.85).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
