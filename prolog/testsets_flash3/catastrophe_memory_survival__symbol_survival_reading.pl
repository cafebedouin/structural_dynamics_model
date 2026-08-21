% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Catastrophe Memory Survival: Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint represents one reading of how Jewish communities preserve
 *   memory and identity after catastrophe: through the strict continuity of
 *   symbolic ritual forms. This 'symbol survival' reading emphasizes the
 *   ritual itself as the primary vehicle for identity, often at the expense
 *   of adapting to modern contexts or recognizing other forms of 'survival'.
 *   The constraint is claimed as a Tangled Rope because it genuinely
 *   coordinates identity and community, but does so with significant
 *   extraction from those who cannot or will not adhere to strict traditional
 *   forms, and requires active enforcement by rabbinic authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Catastrophe Memory Survival: Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'bb273728-cddb-427c-9652-9f856632f66b').
narrative_ontology:cs_kernel_codification('bb273728-cddb-427c-9652-9f856632f66b', formalized).
narrative_ontology:cs_authority_grounding('bb273728-cddb-427c-9652-9f856632f66b', lineage).
narrative_ontology:cs_interpretation_layer_present('bb273728-cddb-427c-9652-9f856632f66b').
narrative_ontology:cs_reading_relation('bb273728-cddb-427c-9652-9f856632f66b', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb273728-cddb-427c-9652-9f856632f66b', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('bb273728-cddb-427c-9652-9f856632f66b', foundational, ritual_form_is_identity).
narrative_ontology:cs_axiom_status(ritual_form_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('bb273728-cddb-427c-9652-9f856632f66b', ritual_form_is_identity, deontological).
narrative_ontology:cs_axiom('bb273728-cddb-427c-9652-9f856632f66b', foundational, continuity_is_survival).
narrative_ontology:cs_axiom_status(continuity_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('bb273728-cddb-427c-9652-9f856632f66b', continuity_is_survival, conventional).
narrative_ontology:cs_reference_frame('bb273728-cddb-427c-9652-9f856632f66b', unbroken_symbolic_chain).
narrative_ontology:cs_drift_state('bb273728-cddb-427c-9652-9f856632f66b', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb273728-cddb-427c-9652-9f856632f66b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, orthodox_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the strict forms and interpretations of ritual, emphasizing symbolic continuity as the core of Jewish survival. Benefits from the authority derived from being the custodians of this tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Find identity, cohesion, and meaning through adherence to traditional ritual forms. Their social and spiritual life is deeply intertwined with the continuity of these practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, orthodox_communities, beneficiary,
    organized, generational, constrained, local).

% Experience a loss of connection to traditional Jewish identity and community when ritual forms are presented as immutable and non-negotiable, leading to cultural alienation or a sense of failure to transmit heritage.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, identity_locked, local).

% Seek to adapt ritual to modern contexts, often prioritizing ethical meaning over strict symbolic form. They bear the cost of being deemed less 'authentic' or 'continuous' by traditional authorities, leading to internal and external disputes over legitimacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, reform_movements, payer,
    organized, generational, constrained, national).

% Analyze the historical evolution of ritual and its various functions, including symbolic and practical aspects. They observe the contest over interpretation without being bound by its internal logic.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and boundary maintenance for Jewish communities by providing shared symbolic experiences and a continuous link to historical memory, particularly in the face of catastrophe.
% TRANSFER_FUNCTION: Transfers cultural capital, social cohesion, and a sense of belonging to adherents, in exchange for strict adherence to prescribed ritual forms and interpretations, from rabbinic authority to communities.
% ABSENT_VOICES: Those who have fully assimilated or abandoned Jewish identity, or those who seek entirely new forms of expression, are absent from the debate over ritual continuity. They would argue that identity can persist without strict adherence to historical forms.
% DISAPPEARANCE_RATIONALE: If the constraint of symbolic ritual continuity vanished, the cohesion and distinctiveness of many Jewish communities would rapidly erode. Identity would become more fluid, and the traditional structures of authority and transmission would collapse, leading to a profound reorganization of Jewish life and memory.
% FOUNDING_PROBLEM: How to preserve Jewish identity, memory, and communal cohesion across generations, especially after catastrophic events like exile, persecution, and the Holocaust, when physical continuity is threatened.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox communities and rabbinic authorities attest that the problem is acutely live, citing ongoing assimilation and external pressures. Historical scholars corroborate the historical necessity of such mechanisms for group survival, though they may dispute the exclusive focus on symbolic form.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the emphasis on strict symbolic continuity imposes significant costs on those who seek alternative expressions of Jewish identity or find traditional forms inaccessible. Suppression (0.65) is also high, as rabbinic authority actively enforces adherence to traditional ritual, often marginalizing or delegitimizing reform efforts. Theater ratio (0.4) reflects that while the symbolic function is real, some of the emphasis on 'continuity' serves to maintain institutional power rather than purely functional transmission. The metrics show a gradual increase in extractiveness and suppression over time as the contest over ritual interpretation intensifies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority, this constraint is a vital Rope, ensuring the very survival of Jewish identity. From the perspective of secularized Jews, it is a Snare, trapping them between an inaccessible tradition and a loss of heritage. The engine's classification as Tangled Rope reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and orthodox communities are beneficiaries, gaining identity and cohesion from the constraint. Secularized Jews and reform movements are victims, bearing the costs of exclusion or delegitimization. Historical scholars act as observers, analyzing the dynamics without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_practical_function,
    'Is the primary function of ritual in catastrophe memory survival purely symbolic, or does it also encode practical survival knowledge?',
    'Ethnographic studies of communities in crisis, historical analysis of ritual adaptation during periods of persecution, and comparative studies of cultural transmission mechanisms.',
    'If practical knowledge is a significant component, the ''symbol_survival_reading'' overstates the purely symbolic function, potentially misclassifying the constraint as more extractive than it is by ignoring a genuine coordination function. If purely symbolic, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_practical_function, empirical, 'Ambiguity regarding the functional scope of ritual in survival.').

omega_variable(
    identity_lock_mechanism,
    'For secularized Jews, is the ''identity_locked'' exit option primarily due to internal psychological attachment to heritage, or external social pressure from traditional communities?',
    'Longitudinal studies of individuals who attempt to disengage from traditional communities, examining the persistence of identity-related distress after reducing social contact.',
    'If primarily internal, the suppression metric might be understated, as the ''lock'' is carried by the individual. If primarily external, the suppression is accurately captured by the social enforcement mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized identity lock for secularized Jews.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the necessity of strict symbolic ritual continuity for Jewish survival a natural law of cultural transmission, or a constructed interpretation that benefits identifiable agents?',
    'Comparative historical analysis of other diasporic cultures that have maintained identity through diverse means, and sociological studies of how ''naturalness'' claims are deployed in religious discourse.',
    'If a constructed interpretation, the constraint''s ''tangled_rope'' classification is robust. If it were a genuine natural law, the classification would shift towards ''mountain'', but the presence of beneficiaries and victims would trigger a false summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Is the constraint a genuine natural law of cultural survival, or a constructed one?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(cata_tr_t1965, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(cata_tr_t1985, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(cata_tr_t2024, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(cata_be_t1965, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(cata_be_t1985, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(cata_be_t2005, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(cata_be_t2024, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(cata_su_t1965, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(cata_su_t1985, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(cata_su_t2005, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(cata_su_t2024, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel. This 'symbol_survival_reading' emphasizes strict symbolic continuity. It is linked to 'competence_transmission_reading' and 'hybrid_encoding_reading' as sibling interpretations of the same core problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
