% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual as Symbolic Boundary-Maintenance and Continuity-of-Practice (Symbol Survival Reading)
 *   domain: religious/collective_memory
 *
 * SUMMARY:
 *   This is one reading of the catastrophe_memory_survival kernel: the claim
 *   that ritual practice preserved Jewish identity and boundary-norms after
 *   catastrophic rupture primarily through symbolic experience, and that
 *   survival should be understood as continuity of ritual practice itself,
 *   rather than as transmission of adaptive practical competence (the
 *   competence_transmission_reading) or as a dual-register system combining
 *   both (the hybrid_encoding_reading). Under this reading, as secularization
 *   and intermarriage have grown across the interval, rabbinic and communal
 *   institutions have increasingly tied recognition, resource allocation, and
 *   belonging to symbolic ritual fidelity, deepening exclusion of those whose
 *   relationship to practice has attenuated — even where those populations
 *   retain other forms of transmitted identity, memory, or resilience. This
 *   story evaluates only the symbol-survival reading's own arrangement; the
 *   sibling readings are separate constraints linked via
 *   network.affects_constraints, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: institutional interpreter and agenda-setter, defines what counts as authentic continuity
 *   - orthodox_communal_institutions: organized beneficiary, collects legitimacy and resources premised on ritual fidelity as the marker of survival
 *   - secularized_jews: constrained payer, coded as discontinuous regardless of other inherited identity
 *   - intermarried_families: trapped payer, excluded by symbolic-boundary rules with no available remedy
 *   - diaspora_youth_disconnected_from_practice: identity-locked payer, burdened with felt inadequacy for non-observance
 *   - academic_and_secular_historians: excluded analytical observers whose competing account of survival is not admitted into communal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual as Symbolic Boundary-Maintenance and Continuity-of-Practice (Symbol Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c').
narrative_ontology:cs_kernel_codification('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', distributed).
narrative_ontology:cs_authority_grounding('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', lineage).
narrative_ontology:cs_interpretation_layer_present('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c').
narrative_ontology:cs_reading_relation('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', foundational, ritual_form_fidelity_constitutes_survival).
narrative_ontology:cs_axiom_status(ritual_form_fidelity_constitutes_survival, holdable).
narrative_ontology:cs_axiom_grounding('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', ritual_form_fidelity_constitutes_survival, conventional).
narrative_ontology:cs_axiom('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', secondary, interpretive_authority_over_practice_is_indispensable_to_continuity).
narrative_ontology:cs_axiom_status(interpretive_authority_over_practice_is_indispensable_to_continuity, holdable).
narrative_ontology:cs_axiom_grounding('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', interpretive_authority_over_practice_is_indispensable_to_continuity, instrumental).
narrative_ontology:cs_reference_frame('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', post_catastrophe_symbolic_reconstitution).
narrative_ontology:cs_drift_state('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', contemporary_diaspora_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9eb32bb8-ed52-4358-a5ac-77c1c32b2b2c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, orthodox_communal_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, diaspora_youth_disconnected_from_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates which ritual forms count as authentic continuity, sets the boundary of who is 'inside' observant practice, and interprets what fidelity to ritual form requires after catastrophe. Its authority is constituted by being the recognized interpreter of the symbolic system — the more that survival is defined as continuity of practice rather than transmission of adaptable competence, the more indispensable its interpretive office becomes.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, beneficiary).

% Synagogues, day schools, and communal bodies whose funding, membership, and legitimacy depend on ritual observance being understood as the substance of survival itself. They collect dues, enrollment, and donor support premised on ritual fidelity as the marker of continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, orthodox_communal_institutions, beneficiary,
    organized, generational, constrained, national).

% Descendants of catastrophe survivors who no longer practice ritual observance in traditional form. Under this reading, their disconnection from ritual practice is coded as a failure of continuity and a loss of identity itself, regardless of whether they retain the practical, familial, or cultural knowledge that ritual once encoded. They bear the cost of being classified as discontinuous, excluded from full communal recognition, and pressured to re-adopt symbolic forms to be counted as having 'survived' authentically.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, constrained, national).

% Families whose composition places them outside strict ritual-boundary categories (matrilineal descent rules, conversion standards, kashrut boundaries). Under the symbol-survival framing their children's status and belonging are contingent on symbolic conformity rather than on any transmitted survival competence, producing exclusion from ritual life, inheritance disputes, and contested burial or marriage rights they cannot resolve by any means available to them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, intermarried_families, payer,
    powerless, biographical, trapped, national).

% Young people raised with partial or no ritual literacy who inherit an obligation to feel loss and inadequacy for not sustaining 'continuity of practice,' even where they possess other forms of inherited identity, memory, or resilience knowledge. Their exit from the symbolic framework is blocked by identity fusion — abandoning ritual is experienced as abandoning ancestors, not as a neutral lifestyle choice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, diaspora_youth_disconnected_from_practice, payer,
    powerless, biographical, identity_locked, global).

% Scholars of Jewish social history and memory studies who argue that post-catastrophe survival depended substantially on practical knowledge transfer (resource networks, migration strategy, adaptive kinship structures) rather than symbolic ritual fidelity alone. Their competing account is marginal to communal religious discourse, cited in academic settings but rarely admitted into the authoritative account of what 'survival' means inside religious institutions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, academic_and_secular_historians, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual practice binds a dispersed, historically persecuted population around a shared, transmissible symbolic repertoire, allowing continuity of collective identity across generations and geographies without requiring centralized political structure.
% TRANSFER_FUNCTION: Moves interpretive authority, communal legitimacy, and resource allocation (education funding, communal recognition, marriage/burial rights) toward those who administer and correctly perform ritual form, and away from those whose relationship to ritual has attenuated — regardless of what other forms of inherited resilience or knowledge they carry.
% ABSENT_VOICES: Secular Jewish communities, intermarried families, and academic historians of practical survival strategy are structurally outside the interpretive body that defines what counts as continuity; they would argue that competence and adaptive knowledge, not symbolic fidelity, is what actually preserved lives and lineages, but this argument does not enter the ritual-authority framework as valid input.
% DISAPPEARANCE_RATIONALE: Rabbinic authority and communal institutions would argue that if the symbol-survival framing disappeared, ritual observance would collapse and Jewish identity would dissolve into assimilation — the world rearranges catastrophically in their account. Secularized and intermarried Jews would argue the opposite: that removing the symbolic-fidelity gatekeeping would simply let already-existing, already-surviving identity and practice be recognized without requiring conformity to a narrower ritual standard — for them, the world is largely unchanged, only the exclusion mechanism disappears. The verdict is genuinely contested between these parties.
% FOUNDING_PROBLEM: After catastrophic rupture (expulsion, pogrom, genocide), a dispersed and traumatized population needed some mechanism to maintain continuous collective identity across generations without political sovereignty or geographic unity — ritual practice was built to be that mechanism, encoding boundary-norms into repeatable symbolic form.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic and communal institutional voices attest the founding problem is still fully live — assimilation and discontinuity remain existential threats requiring strict ritual fidelity. Independent memory-studies scholars and sociologists of American and European Jewish life (outside the benefiting institutions) attest that the founding problem has partially shifted: population continuity now depends heavily on factors ritual-fidelity framing does not track (intermarriage integration, cultural literacy, diasporic solidarity networks), and that treating ritual form as the sole proxy for survival produces exclusion without correspondingly improving continuity outcomes.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) is authored high because the symbol-survival reading ties material and social goods — communal recognition, marriage/burial rights, educational access, felt legitimacy of identity — to a specific, contestable standard (ritual fidelity) rather than to a broader and more defensible measure of what actually sustained the population after catastrophe. Suppression (0.58) reflects genuine boundary-enforcement mechanisms (matrilineal descent rules, conversion gatekeeping, exclusion from communal ritual life) but is lower than extraction because much of the enforcement operates through internalized identity-fusion rather than external coercive machinery. Theater ratio (0.42) is elevated and rising because a growing share of ritual-fidelity enforcement functions to perform continuity for institutional and donor audiences rather than to track any operative survival mechanism — a signal this reading should be watched for further Goodhart drift.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority and communal institution seats, the arrangement is coordination: a shared symbolic system holding a scattered population together across generations. From the secularized, intermarried, and diaspora-youth seats, the same structure operates as an imposed and increasingly costly filter that penalizes non-conformity to ritual form even where the population's actual continuity (via memory, kinship networks, and adaptive practice) persists. The engine should compute a tangled_rope reading from the agenda-setter seat's own metrics diverging sharply from the payer seats' experienced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and communal institutions sit near the beneficiary end: they administer the interpretive standard and collect legitimacy, funding, and institutional relevance from its operation. Secularized Jews, intermarried families, and diaspora youth sit near the target end: they bear exclusion, contested status, and internalized inadequacy as the direct cost of the standard's operation, with limited or no exit (trapped/identity_locked) because leaving the framework is experienced as abandoning inherited identity, not as a neutral choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining collective identity without political sovereignty after catastrophic rupture — was genuinely live at founding. Under this reading it is contested rather than resolved: rabbinic authority insists the problem remains as acute as ever, while independent historians and sociologists argue the population's actual continuity now depends on factors the ritual-fidelity standard does not capture, meaning the standard risks functioning as a self-perpetuating mandate (mandatrophy) that persists past its diagnostic usefulness. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the entire ritual system as pure extraction (it does perform genuine, non-trivial coordination for those actively engaged in observant life) and treating it as an untouchable natural fact (it is a contestable interpretive standard with identifiable beneficiaries and victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_competence_survival_boundary,
    'Did post-catastrophe Jewish population continuity depend primarily on symbolic ritual continuity, primarily on transmitted practical competence, or on both inseparably?',
    'Comparative historical and sociological study of communities that retained ritual form but lost practical adaptive networks (and vice versa), tracking actual continuity outcomes (demographic persistence, intergenerational identity retention) against each factor independently.',
    'If competence transmission was the operative mechanism, this reading''s high extractiveness attribution to ritual-fidelity gatekeeping is largely unwarranted rent-seeking by interpretive authority; if symbolic continuity was genuinely load-bearing on its own, the coordination function is more substantial than the victim-set framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_vs_competence_survival_boundary, empirical, 'Whether ritual''s symbolic function or embedded practical competence was the operative survival mechanism.').

omega_variable(
    interpretive_authority_natural_vs_constructed,
    'Is rabbinic interpretive authority over what counts as authentic continuity a natural consequence of religious tradition''s internal logic, or a constructed position that concentrates benefit by defining survival in terms only it can adjudicate?',
    'Historical analysis of how interpretive authority over ''authentic practice'' shifted across periods of persecution and stability — does the scope of rabbinic gatekeeping expand specifically during periods when institutional resources and legitimacy are threatened?',
    'If authority scope tracks institutional self-interest rather than doctrinal continuity, this strengthens the tangled_rope/extraction reading over a benign-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_natural_vs_constructed, conceptual, 'Whether rabbinic interpretive authority is doctrinally necessitated or self-reinforcing.').

omega_variable(
    identity_lock_suppression_mechanism,
    'For diaspora youth and secularized Jews, is the suppression that keeps them oriented toward ritual-fidelity standards primarily structural (communal exclusion, family pressure, institutional gatekeeping) or internalized (inherited guilt, identity fusion with ancestral memory)?',
    'Longitudinal interviews tracking whether felt inadequacy about non-observance persists after individuals fully exit observant communal contexts and lose structural exposure to communal pressure.',
    'If suppression is substantially internalized, effective suppression is higher than the structural measure suggests, and the identity_locked exit classification for diaspora youth is validated rather than overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for identity-locked descendants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 14, 0.26).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 28, 0.31).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 42, 0.35).
narrative_ontology:measurement(cata_tr_t56, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 56, 0.39).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 28, 0.57).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 42, 0.62).
narrative_ontology:measurement(cata_be_t56, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 56, 0.66).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 14, 0.46).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 42, 0.53).
narrative_ontology:measurement(cata_su_t56, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 56, 0.56).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_survival kernel, decomposed per the ε-invariance principle because 'ritual and survival' names structurally distinct claims with different ε values. The symbol_survival_reading authors the highest ε of the three (0.68) because it ties survival most tightly and exclusively to a single contestable interpretive standard administered by a concentrated authority. The competence_transmission_reading should author substantially lower ε (practical knowledge transfer has diffuse, non-gatekept beneficiaries). The hybrid_encoding_reading should sit between the two. All three share the same kernel_id (catastrophe_memory_survival) but are authored as separate constraint files, each with its own stakeholders, metrics, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
