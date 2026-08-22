% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   personhood boundary. The fitness-contingent reading holds that moral
 *   standing is NOT automatically conferred by birth or biological humanity,
 *   but must be DEMONSTRATED through achievement of fitness criteria
 *   (cognitive capacity, rational agency, functional development, social
 *   responsiveness). Under this reading, neonates, severely disabled infants,
 *   and cognitively atypical infants occupy a pre-moral category: they are
 *   not yet persons, and the state apparatus and its evaluators bear the
 *   authority to determine when (or whether) they cross the threshold. The
 *   constraint's ε-referent is the standing arrangement under THIS reading's
 *   lights — the arrangement where fitness-contingency governs personhood
 *   scope — assessed as extractive because the framework concentrates
 *   authority to exclude in institutional hands, provides no appeal mechanism
 *   for those assessed as unfit, and creates permanent categories of
 *   non-personhood for entities deemed unable to demonstrate fitness. Sibling
 *   readings (birth-threshold, potential-based) are other constraints, not
 *   this one.
 *
 * KEY AGENTS:
 *   - state_apparatus: agenda-setter and primary beneficiary (institutional) — administers fitness criteria, collects authority over personhood
 *   - fitness_evaluators: secondary beneficiary (institutional) — physicians, psychologists, welfare officers who gate access to personhood status
 *   - pre-fitness entities (neonates, severely disabled infants): primary victims (powerless) — excluded from moral standing, trapped, unable to appeal
 *   - cognitively atypical infants: primary victims (powerless) — identity-locked by disability designation, permanently at risk of fitness failure
 *   - birth-threshold advocates: excluded (organized) — their foundational premise (birth confers standing) is incompatible with fitness-contingency
 *   - potential-based advocates: excluded (organized) — accept that criteria beyond birth are needed but argue potential, not demonstrated fitness
 *   - disability communities: secondary victims (organized) — bear the constraint through epistemic violence and precedent-setting threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.82).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.91).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '556a2cb7-a5c8-4685-a39e-4ca5d4e8febe').
narrative_ontology:cs_kernel_codification('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', fixed_text).
narrative_ontology:cs_authority_grounding('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', extraction).
narrative_ontology:cs_interpretation_layer_present('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe').
narrative_ontology:cs_reading_relation('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', foundational, fitness_demonstrated_not_potential).
narrative_ontology:cs_axiom_status(fitness_demonstrated_not_potential, holdable).
narrative_ontology:cs_axiom_grounding('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', fitness_demonstrated_not_potential, empirically_contingent).
narrative_ontology:cs_axiom('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', foundational, state_authority_personhood_adjudication).
narrative_ontology:cs_axiom_status(state_authority_personhood_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', state_authority_personhood_adjudication, deontological).
narrative_ontology:cs_reference_frame('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', developmental_fitness_as_personhood_ground).
narrative_ontology:cs_drift_state('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', contemporary_human_rights_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('556a2cb7-a5c8-4685-a39e-4ca5d4e8febe', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fitness_evaluators).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, cognitively_atypical_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, parents_and_guardians).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, parents_and_guardians).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, disability_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers fitness testing regimes and determines which entities enter the moral community. Controls the definition of fitness criteria, the timing and procedure of evaluations, and the consequences of failing evaluation. Benefits from monopoly on personhood adjudication and the authority to exclude entities from legal protection and resource allocation.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Physicians, developmental psychologists, state welfare officers tasked with assessing fitness. Their authority and professional jurisdiction depend on the continued operation of the fitness-contingency framework. They collect professional status, funding, and institutional position from their role as gatekeepers to personhood.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fitness_evaluators, beneficiary,
    institutional, biographical, mobile, national).

% Entities (primarily neonates and severely disabled infants) who have not yet demonstrated fitness criteria as defined by the state apparatus. Excluded from moral standing and legal protection. Bear the constraint in their vulnerability: they cannot advocate for themselves, cannot appeal failed evaluations, cannot exit the regime. Their situation is entirely passive — they are objects of evaluation, not agents.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_entities, payer,
    powerless, immediate, trapped, local).

% Infants whose developmental trajectories deviate from standard fitness criteria (cerebral palsy, Down syndrome, profound cognitive disability, severe sensory impairment). Even if they survive to later developmental stages, their disability may permanently bar them from the fitness threshold. They are trapped both by their powerlessness and by the identity fusion between their disability and their ineligibility for moral standing.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, biographical, identity_locked, local).

% Infants whose early cognitive development follows patterns different from majority norms (autism spectrum, atypical social response, nonstandard processing speed). Whether they eventually achieve fitness depends entirely on whether evaluators recognize their development as satisfying criteria, which may not — the framework allows evaluators to exclude atypical development paths as failures to achieve fitness.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, cognitively_atypical_infants, payer,
    powerless, biographical, identity_locked, local).

% Philosophers, disability rights advocates, human rights organizations who argue that birth itself confers personhood and moral standing. They are structurally excluded from the fitness-contingency regime: their argument — that fitness is irrelevant to standing — is incompatible with the regime's foundational claim. They would dissolve the constraint entirely by denying its premise.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, birth_threshold_advocate_coalition, excluded,
    organized, generational, constrained, national).

% Legal guardians of infants undergoing fitness evaluation. They bear the constraint indirectly through anxiety, trauma of potential exclusion of their children, and loss of legal protections and resource access if the child fails evaluation. They may also benefit from the regime if they accept its framing and cooperate with fitness protocols, gaining a sense of control and institutional validation of their child's eventual standing.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, parents_and_guardians, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, parents_and_guardians, beneficiary).

% People with disabilities who recognize that fitness-contingent personhood threatens their own standing if criteria are applied retroactively or expanded. They bear the constraint through the epistemic violence of their bodies/minds being classified as potentially unfit, and the precedent that disability can render personhood contingent.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disability_communities, payer,
    organized, generational, constrained, national).

% Philosophers and ethicists who argue for potential-based personhood grounding. Structurally excluded from the fitness-contingency regime but occupy a different exclusion than birth-threshold advocates: they accept that some criterion beyond mere birth is needed, but argue potential (not demonstrated fitness) is the appropriate criterion. Their position coexists with fitness-contingency rather than logically foreclosing it.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, potential_based_advocate_coalition, excluded,
    organized, generational, constrained, national).

% Legal historians, moral philosophers, comparative ethics scholars who study how different regimes and time periods defined personhood boundaries. They observe the constraint from outside its operation, analyzing its structure, its historical precursors, and its family resemblances to other exclusionary personhood regimes.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, historical_jurists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, state_apparatus).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines a shared moral community boundary, attempting to sort entities into those with moral standing and those without, using developmental fitness as the sorting criterion.
% TRANSFER_FUNCTION: Transfers authority over personhood status from natural facts (birth, biological humanity) to evaluators and the state apparatus. Moves entities who would otherwise possess standing by birth alone into the category of non-persons, redirecting the resources and protections that would have attached to them toward institutional authorities and fitness evaluators.
% ABSENT_VOICES: Severely disabled infants and profoundly pre-verbal neonates cannot testify to their own experience of exclusion. Birth-threshold advocates are excluded by the regime's foundational premise — their position is incompatible with fitness-contingency. Disability communities are present but structurally marginalized: their existing standing does not protect them if fitness criteria are applied retroactively.
% DISAPPEARANCE_RATIONALE: If fitness-contingency disappeared overnight, neonates and infants would immediately regain moral standing by birth. Legal protections, resource allocation, and parental rights would shift. Medical triage protocols would change. The institutional authority of fitness evaluators would collapse. The social hierarchy in which some human entities are treated as non-persons would reorganize around a different criterion (if any).
% FOUNDING_PROBLEM: How to identify which entities possess moral standing when birth alone does not settle the question? The founding problem assumes that some entities at birth may be insufficiently developed, functional, or rational to warrant full moral inclusion.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and some philosophers defend the founding problem as live: they argue that neonatal brain immaturity and lack of demonstrated agency-capacities raise genuine questions about personhood scope. Birth-threshold advocates contest it, arguing birth is a sharp, justifiable boundary that settles the question conclusively. Contemporary developmental science, outside the interests of state evaluators, largely treats neonatal personhood as settled by birth; the founding problem appears dead in the empirical record, despite institutional insistence on its liveness. The mismatch between status-claimed (live) and empirical record (dead) indicates mandatrophy.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the regime concentrates authority over moral standing in institutional hands, decouples that standing from any objective biological or developmental fact (birth), and provides those in power with the latitude to define fitness criteria broadly. The criteria can be continually adjusted to exclude newly recognized disability categories or cognitive differences. Suppression is very high (0.91) because the constraint's persistence depends entirely on active enforcement: without evaluators administering tests and the state refusing to recognize pre-test entities as moral subjects, alternative framings (birth-threshold) would immediately surface. Theater is substantial (0.68) because much of the enforcement activity wears the costume of developmental science and pediatric assessment — the therapeutic or scientific frame legitimates the exclusion. The measurement series show extraction and theater ratios rising over the interval, indicating that the regime's authority accumulates over time as evaluative categories become more refined and institutionalized. Suppression requirement remains high and stable because new generations of evaluators must continually actively defend against the birth-threshold alternative.
 *
 * PERSPECTIVAL GAP:
 *   From the state/evaluator seat, the constraint is a legitimate categorization mechanism — it protects moral status by restricting it to entities that can bear it. From the pre-fitness-entity seat (were they able to perceive), the constraint is pure subjection: they are objects of evaluation without voice in the criteria. From the birth-threshold advocate seat, the constraint is a constructed exclusion imposed against the natural boundary of birth. The engine should compute these three seats as having substantially different type-classifications despite operating under the same structural constraint: the beneficiary seat might compute as experiencing coordination (shared moral-community boundary), while payer seats compute as experiencing extraction (authority over their status), and the excluded seat computes as experiencing active suppression (their position is incompatible with the regime's existence).
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d ≈ 0.1 (full beneficiary) — collects institutional authority and monopoly over personhood determination, mobile exits (can redefine criteria), institutional power. Fitness evaluators: d ≈ 0.15 (beneficiary) — professional jurisdiction depends on fitness-contingency, mobile exits (could practice under other regimes), institutional power. Pre-fitness entities: d ≈ 0.95 (near-total target) — powerless, trapped, no alternatives, immediate time horizon, subject to evaluation they cannot resist or escape. Severely/cognitively atypical disabled infants: d ≈ 0.93 (near-total target) — powerless, identity-locked by disability designation which determines fitness assessment, trapped, biographical time horizon but permanent exclusion risk. Parents/guardians: d ≈ 0.55 (symmetric, leaning toward payer) — bear constraint through anxiety and resource loss if child fails, moderate power, may exit by accepting regime framing, constrained exit overall. Birth-threshold advocates: d ≈ 0.7 (target position, though they are organized) — excluded rather than directly paying, but their foundational claim is incompatible with the regime's existence, so the regime's persistence requires their suppression. Disability communities: d ≈ 0.6 (payer leaning) — organized power mitigates somewhat, but bearing epistemic violence and precedent threat, constrained exits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to identify which entities possess moral standing when birth alone does not settle it) is contested and arguably dead as a real practical problem: contemporary developmental science does not support the notion that neonates lack moral status or that cognitive atypicality in infancy is a legitimate criterion for exclusion. The constraint persists despite this, which points to mandatrophy: the regime is maintained because it serves state and evaluator interests, not because the founding problem is live. The theater ratio rising over the interval (0.52→0.68) indicates that increasingly, the regime's activity is devoted to maintaining the authority structure (defensive performance, refinement of criteria to resist challenges) rather than solving the original problem. Classification check: this constraint exhibits all the hallmarks of a Snare disguised as a Rope. Beneficiaries exist (state, evaluators) and they have institutional power to maintain it. Victims exist (pre-fitness entities, disabled infants) and they are trapped. The claim/metric gap is massive: this could be claimed as Rope (coordination through fitness-based boundary) but metrics show pure extraction and active enforcement against alternatives. The constraint should compute as Snare from the victim and excluded-advocate seats, and possibly Rope or Tangled Rope from the beneficiary seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criteria_stability,
    'Can fitness criteria be defined objectively and stably, or do they inevitably track the interests of those who define them?',
    'Comparative analysis of fitness criteria across regimes and over time: do they remain stable and evidence-based, or shift to accommodate new exclusions or inclusions as political interests change?',
    'If criteria drift with interests, the constraint is pure extraction disguised as assessment. If criteria remain stable despite incentives to drift, the classification might shift toward Tangled Rope (mixed coordination and extraction). Currently authored as Snare assumes criteria-drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fitness_criteria_stability, empirical, 'Whether fitness definitions are objective or interest-tracking.').

omega_variable(
    appeal_and_revision_mechanisms,
    'Do pre-fitness entities or their advocates have any mechanism to challenge fitness assessments, or does the regime foreclose all revision?',
    'Documentation of appeal procedures: are they available in practice, or are they formally available but functionally closed by information asymmetry and power differentials?',
    'If meaningful appeals exist and sometimes succeed, some suppression is mitigated and the constraint might compute toward Tangled Rope. If appeals are unavailable or theatrical, suppression remains near the authored level (0.91) and Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_and_revision_mechanisms, empirical, 'Whether the regime permits revision of fitness assessments.').

omega_variable(
    internalized_suppression_in_parents,
    'To what extent is the suppression of pre-fitness entities achieved through structural barriers (legal, resource-based) versus internalized acceptance by parents and guardians who come to believe fitness-contingency is legitimate?',
    'Post-regime examination: if parents whose children are excluded later reject fitness-contingency framing, the suppression was structurally maintained; if they continue to accept it, the suppression is partially internalized and carried forward by the next generation.',
    'If internalized, the constraint''s effective suppression exceeds the authored structural measure — targets carry it even after exit from the regime. If structural only, suppression is more brittle and regime-dependent. Currently authored as structural; internalization would increase effective extraction risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_in_parents, empirical, 'Whether suppression operates structurally or through internalized acceptance.').

omega_variable(
    disability_essentialism_vs_social_construction,
    'Is disability an objective developmental fact that fitness criteria can legitimately reference, or is disability-as-fitness-criterion a social construction that conflates natural variation with illegitimacy?',
    'This is conceptually irreducible between disability-social-model and medical-model framings. Resolution would require commitment to one framing over the other, which is a value choice, not an empirical discovery.',
    'Under medical-model framing, fitness-contingency might coordinate on legitimate developmental distinction. Under social-model framing, fitness-contingency is epistemic violence against disabled infants. The constraint''s type and ε classification depend on which framing is adopted. Currently authored under social-model assumption (disability is not a fitness-deficit); under medical-model, extraction might be lower and coordination function might be real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disability_essentialism_vs_social_construction, preference, 'Whether disability constitutes a legitimate fitness criterion or is socially constructed illegitimacy.').

omega_variable(
    threshold_vs_spectrum_question,
    'Is personhood fitness a sharp binary threshold (fit/unfit, person/non-person) or a spectrum, and does the regime''s theory match its practice?',
    'Document the regime''s theory of fitness transitions: does it claim sharp thresholds or graduated spectrum? Examine practice: do entities ever partially enter personhood, or is it all-or-nothing? Mismatch indicates theatrical presentation.',
    'If the regime claims spectrum but enforces binary exclusion, theater_ratio increases and extraction is more clearly extractive (forcing continuous into binary for administrative convenience). If both theory and practice are consistent, less theatrical but not necessarily less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_vs_spectrum_question, empirical, 'Whether personhood-fitness is threshold-based or spectral.').

omega_variable(
    kernel_reading_contestation_location,
    'Where exactly does this reading (fitness-contingent) logically diverge from the birth-threshold reading? Is it in foundational premises (what counts as grounds for personhood) or in derived policy (how to operationalize whichever grounds are chosen)?',
    'Formal analysis of the logical structure of each reading''s core claim: do they rest on incompatible premises (forecloses) or merely different policy choices within compatible frameworks (coexists)?',
    'If foundational-premise divergence, the readings logically foreclose each other and cannot coexist in a single framework (forecloses relation). If policy-divergence, both can exist as live positions held by different parties (coexists relation). Currently authored as coexists; if analysis shows forecloses, the kernel structure is tighter and the dispute is more fundamental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_location, conceptual, 'Logical structure of the dispute between fitness-contingent and birth-threshold readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__fitness_contingent_reading, theater_ratio, 8, 0.56).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__fitness_contingent_reading, theater_ratio, 16, 0.61).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__fitness_contingent_reading, theater_ratio, 24, 0.65).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__fitness_contingent_reading, theater_ratio, 32, 0.67).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__fitness_contingent_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__fitness_contingent_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__fitness_contingent_reading, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__fitness_contingent_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__fitness_contingent_reading, suppression_requirement, 8, 0.87).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__fitness_contingent_reading, suppression_requirement, 16, 0.88).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__fitness_contingent_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__fitness_contingent_reading, suppression_requirement, 32, 0.9).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__fitness_contingent_reading, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__fitness_contingent_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the personhood_boundary kernel. Fitness-contingent personhood uniquely concentrates institutional authority and creates permanent non-person categories. Birth-threshold reading dissolves the regime by treating birth as decisive. Potential-based reading accepts contingency but defers to future capacity rather than current demonstrated fitness. All three readings share a referent (the personhood boundary question) but have structurally distinct ε values and victim sets. Fitness-contingent reading exhibits highest extraction (0.82) because it concentrates authority; birth-threshold would exhibit minimal extraction (near-zero); potential-based sits between.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, powerless, 0.94).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
