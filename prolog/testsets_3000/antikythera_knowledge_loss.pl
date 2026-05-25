% ============================================================================
% CONSTRAINT STORY: antikythera_knowledge_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antikythera_knowledge_loss, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antikythera_knowledge_loss
 *   human_readable: Loss of Hellenistic Precision Gearing Knowledge
 *   domain: technological/epistemic
 *
 * SUMMARY:
 *   The Antikythera Mechanism represents a catastrophic knowledge loss in
 *   Hellenistic mechanical technology. A sophisticated analog computer for
 *   astronomical calculation, built around 100 BCE with precision bronze
 *   gearing and complex gear-trains, demonstrates mechanical and mathematical
 *   sophistication that would not be matched again in Europe for over 1,500
 *   years. The loss of this knowledge was not inevitable — it resulted from
 *   institutional collapse (destruction of libraries, end of patronage
 *   systems), trade disruption (loss of Mediterranean commerce networks), and
 *   fragmentation of the technical tradition. The constraint is that this
 *   lost knowledge created a structural suppression on technological
 *   progress: without the conceptual framework and material knowledge of
 *   precision gearing, subsequent civilizations had to rediscover fundamental
 *   mechanical principles from first principles. The mechanism itself was
 *   lost until its recovery from a shipwreck in 1901, creating a curious
 *   inversion — the artifact survived, but its meaning and the knowledge
 *   tradition it represented did not. Modern reconstruction efforts represent
 *   a scaffold-type response: reverse-engineering the artifact to recover
 *   principles and reintegrate them into contemporary mechanical knowledge.
 *   However, the three omegas reveal deep uncertainties about the extent,
 *   causality, and completeness of the knowledge loss.
 *
 * KEY AGENTS:
 *   - Hellenistic Technical Tradition: Primary victim (powerless/trapped) — knowledge holder that cannot transmit or preserve itself once institutional supports collapse
 *   - Hellenistic Mechanical Knowledge Commons: Primary victim (powerless/trapped) — abstract epistemic resource with no advocate or exit option; extracted by institutional collapse
 *   - Mediterranean Technical Practitioners: Secondary victim (moderate/trapped) — skilled craftspeople lose access to knowledge base and cannot recover it within their lifetimes
 *   - Byzantine and Islamic Mechanical Traditions: Institutional actors (institutional/constrained) — preserve modified mechanical knowledge (water mills, astrolabes) but lack original precision-gearing principles; maintain theater through degraded versions
 *   - Modern Archaeological and Engineering Research: Analytical observer (analytical/analytical) — reverse-engineer principles from artifact; create scaffold for knowledge recovery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_knowledge_loss, 0.58).
domain_priors:suppression_score(antikythera_knowledge_loss, 0.72).
domain_priors:theater_ratio(antikythera_knowledge_loss, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_knowledge_loss, extractiveness, 0.58).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antikythera_knowledge_loss, snare).
narrative_ontology:human_readable(antikythera_knowledge_loss, "Loss of Hellenistic Precision Gearing Knowledge").
narrative_ontology:topic_domain(antikythera_knowledge_loss, "technological/epistemic").

domain_priors:requires_active_enforcement(antikythera_knowledge_loss).
% --- Structural relationships ---
narrative_ontology:constraint_victim(antikythera_knowledge_loss, hellenistic_technical_tradition).
narrative_ontology:constraint_victim(antikythera_knowledge_loss, mechanical_knowledge_continuity).
narrative_ontology:constraint_victim(antikythera_knowledge_loss, technological_progress_mediterranean).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The collective knowledge tradition cannot exit the suppression regime. Once precision gearing techniques are lost, no mechanism exists to recover them without independent reinvention. Trapped in the suppression state with maximum extraction from potential technological progress.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MEDITERRANEAN TECHNICAL PRACTITIONERS (SNARE) — Skilled craftspeople and engineers in post-Hellenistic Mediterranean have no pathway to recover lost gear-cutting knowledge. Library destruction, trade disruption, and institutional collapse eliminate transmission mechanisms. High suppression, no exit options despite moderate organizational capacity.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: BYZANTINE AND ISLAMIC MECHANICAL TRADITIONS (PITON) — These institutional knowledge holders preserve water-mills, astrolabes, and mechanical clocks but perform degraded versions of original Hellenistic principles. Theater ratio is high because the institutions maintain the appearance of technical sophistication through modified, less-precise designs. The underlying precision-gearing knowledge remains lost; institutional memory preserves only the accessible subset.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: REVERSE-ENGINEERING RESEARCH (SCAFFOLD) — Modern materials analysis, CT scanning, and mechanical simulation enable reconstruction of lost principles from artifact examination. This is a temporary support structure with a sunset clause: once the mechanism is fully understood and principles are reintegrated into modern mechanical knowledge, the constraint loses force. The suppression was contingent on specific historical circumstances (institutional collapse, material loss) that modern technology can circumvent.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational view, the loss of knowledge is irreversible once all materials and knowledge-holders perish. The constraint appears as a natural law of information: in the absence of external storage (written records, material artifacts), knowledge decays exponentially toward zero. The Antikythera Mechanism is the exception that proves the rule — surviving artifact allowed later recovery. But the vast majority of lost Hellenistic knowledge has no artifact record and remains permanently inaccessible. This perspective risks naturalizing historical contingency as law.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_knowledge_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antikythera_knowledge_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antikythera_knowledge_loss, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antikythera_knowledge_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(antikythera_knowledge_loss, TR),
    TR >= 0.70.

:- end_tests(antikythera_knowledge_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The knowledge loss created persistent suppression on technological progress for over a millennium. The mechanism of extraction is structural: once the knowledge tradition collapses, no agent has incentive to recover or reinvent the precise gear-cutting techniques required for comparable mechanisms. The extractiveness increased from 0.28 (early Hellenistic period, when knowledge was active and partially documented) to 0.58 after institutional collapse. The value reflects that 75% of potential technological progress from precision-mechanical knowledge was forgone during the interval. Suppression (0.72): Very high. Multiple barriers prevented knowledge recovery: (1) Material loss — the artifact was destroyed (until 1901 recovery); (2) Institutional collapse — schools and libraries were destroyed; (3) Textual loss — treatises on gear-cutting were lost; (4) Social reorganization — shift away from precision manufacturing incentives; (5) Tacit knowledge loss — master craftspeople died without apprentices. Suppression reflects both the severity of institutional barriers and the absence of alternatives. Theater ratio (0.68): High. The Byzantine and Islamic mechanical traditions maintained the appearance of technical sophistication (astrolabes, water mills) while performing degraded versions of Hellenistic principles. This is classic piton theater — the institutions maintained their status as knowledge-holders through performative preservation of simplified designs, masking the loss of underlying precision principles.
 *
 * PERSPECTIVAL GAP:
 *   The epistemic commons and technical practitioners experience this as maximum suppression (Snare) — they have no exit and no mechanism for knowledge recovery within the 1,500-year interval. Byzantine and Islamic institutions experience it as piton degradation — they maintain institutional knowledge of mechanical devices but through simplified, less-precise designs that preserve the appearance of technical tradition while losing the substance. Modern reverse-engineering research experiences it as a temporary scaffold — the artifact has survived, and contemporary materials analysis can reconstruct principles, creating a sunset mechanism where the constraint loses force as knowledge is reintegrated. The analytical observer risks seeing this as a natural law of knowledge loss (Mountain), but the structural data reveals it as contingent institutional failure — the artifact's survival was chance, and the knowledge was recoverable only because the artifact survived.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no beneficiaries in this constraint — no agent benefits from the knowledge loss. The structure is pure extraction from victims with no exit. The epistemic commons is trapped because knowledge loss is irreversible without external intervention (the artifact recovery). Practitioners are trapped because they cannot rediscover complex principles within biographical timescales. Institutional actors experience constraint rather than trapping because they have some exit through simplified designs (the piton perspective), but this is degradation rather than genuine exit. The analytical observer has arbitrage options (modern materials science) that would-be Hellenistic engineers lacked. D values are very high for trapped agents (d ≥ 0.90), reflecting maximum extraction with zero exit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED by showing that this is a genuine Snare (not misclassified as pure coordination). The constraint has zero coordination function — it serves no beneficial purpose for any agent. No beneficiary declaration is needed because no agent structurally benefits from knowledge loss. The absence of beneficiaries itself is diagnostic: pure extraction with no coordination hiding place. The piton perspective (Byzantine/Islamic) reveals the theater mechanism — maintained institutions preserve the appearance of knowledge while performing degraded versions, creating the false impression that knowledge continuity persists when fundamental principles are lost. The scaffold perspective reveals the sunset mechanism — modern reverse-engineering can recover lost principles, but only through external technology (CT scanning, computational modeling) unavailable to post-Hellenistic civilizations. The mandatrophy resolves by showing that extractiveness (0.58) genuinely reflects the suppression (0.72) and theater (0.68) — this is a snare precisely because no coordination benefit masks the extraction, and the theater is not concealing coordination but degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_complexity_threshold,
    'Was the Antikythera Mechanism''s technology a isolated sophisticated anomaly or the visible peak of a broader Hellenistic precision-engineering tradition?',
    'Archaeological survey for additional precision-gearing artifacts; textual analysis of Hellenistic technical literature (Hero of Alexandria, Archimedes treatises) for evidence of systematic gear-cutting knowledge; metallurgical analysis of period machinery for evidence of precision fabrication techniques',
    'If isolated anomaly (ε ≤ 0.30): constraint is Rope — mechanical knowledge was never widely distributed and loss was low-extraction. If peak of tradition (ε = 0.58+): constraint is Snare — widespread suppression of a technical knowledge base that would have enabled continuous technological progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_complexity_threshold, empirical, 'Whether Antikythera represents an anomaly or peak of broader tradition').

omega_variable(
    institutional_collapse_causality,
    'How much of knowledge loss was due to institutional collapse (loss of schools/libraries) versus active suppression (political decision to restrict technology) versus economic reorganization (shift away from precision manufacturing incentives)?',
    'Historical analysis of knowledge preservation in Byzantine and Islamic institutions; textual evidence of deliberate technological restriction or preservation policies; economic analysis of post-Hellenistic trade patterns and manufacturing investment',
    'If collapse-driven (passive): suppression value ≤ 0.40 — loss was incidental, not enforced. If suppression-driven: suppression ≥ 0.70 — constraint becomes active snare. If economically reorganized: beneficiary analysis required — some institutional actors may have benefited from shift away from precision manufacturing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_collapse_causality, empirical, 'Causality of knowledge loss: collapse, suppression, or economic reorganization').

omega_variable(
    recovery_completeness,
    'Can modern reverse-engineering of the Antikythera Mechanism fully restore the original Hellenistic principles, or is some tacit knowledge (master craftsperson techniques, material properties, precision requirements) permanently lost even with the artifact?',
    'Longitudinal study of reconstruction attempts (Freeth et al., Aidinis, others); comparison of reconstructed device performance with inferred original specifications; attempt to independently derive gear-cutting techniques from geometric analysis alone',
    'If complete recovery: scaffold sunset is achievable — knowledge can be reintegrated. If permanent gaps: constraint remains partially in force — reconstructed knowledge will always differ from original, creating a new Snare layer (the myth of complete recovery masking persistent gaps).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_completeness, empirical, 'Whether modern reverse-engineering can fully restore original Hellenistic principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antikythera_knowledge_loss, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antik_tr_t0, antikythera_knowledge_loss, theater_ratio, 0, 0.15).
narrative_ontology:measurement(antik_tr_t750, antikythera_knowledge_loss, theater_ratio, 750, 0.68).
narrative_ontology:measurement(antik_tr_t1500, antikythera_knowledge_loss, theater_ratio, 1500, 0.68).

% Extraction over time
narrative_ontology:measurement(antik_be_t0, antikythera_knowledge_loss, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(antik_be_t750, antikythera_knowledge_loss, base_extractiveness, 750, 0.58).
narrative_ontology:measurement(antik_be_t1500, antikythera_knowledge_loss, base_extractiveness, 1500, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antikythera_knowledge_loss, information_standard).
narrative_ontology:affects_constraint(antikythera_knowledge_loss, mediterranean_trade_disruption).
narrative_ontology:affects_constraint(antikythera_knowledge_loss, library_destruction_late_antiquity).
narrative_ontology:affects_constraint(antikythera_knowledge_loss, patronage_collapse_hellenistic_institutions).

% DUAL FORMULATION NOTE:
% The Antikythera knowledge loss decomposes into three upstream constraints: (1) Mediterranean Trade Disruption (ε=0.35, Rope to Snare transition) — loss of commerce networks that transmitted technical knowledge; (2) Library Destruction in Late Antiquity (ε=0.42, Snare) — institutional collapse eliminating knowledge storage; (3) Patronage Collapse of Hellenistic Institutions (ε=0.40, Tangled Rope) — shift from patronage systems that funded precision research. Each upstream constraint independently weakened knowledge transmission; their combined effect created the severe suppression (0.72) in the knowledge loss constraint. This story models the aggregate constraint experienced by the epistemic commons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(antikythera_knowledge_loss, analytical, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
