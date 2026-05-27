% ============================================================================
% CONSTRAINT STORY: living_drift_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_drift_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_drift_reading
 *   human_readable: Living Drift Reading: Latin Correctness as Continuous Institutional Practice
 *   domain: historical_linguistics/philology/renaissance_studies
 *
 * SUMMARY:
 *   The living drift reading of Latin correctness instantiates a specific
 *   authority claim: the correct form of Latin is the form actually used by
 *   competent speakers and writers in continuous institutional practice, not
 *   the form preserved in canonical classical texts. This reading emerged as
 *   the dominant institutional norm in medieval Christianity (roughly
 *   6th–11th centuries) and persisted through the high medieval period
 *   despite humanist challenges in the Renaissance. The reading's core
 *   commitment is that linguistic correctness derives from usage ('usus est
 *   norma loquendi') — the practices of practicing scholars, clerics, and
 *   institutional writers establish standards through their cumulative
 *   choices. Under this reading, new vocabulary enters legitimately when
 *   institutional needs require it (theological neologisms, medical
 *   terminology), pronunciation drifts toward vernacular patterns for
 *   pedagogical efficiency, and orthography reflects speech. The constraint
 *   exhibits moderate extractiveness (0.38) because the reading both enables
 *   rapid institutional adaptation and suppresses alternatives (textual
 *   purist claims for canonical authority). Theater has increased over the
 *   measurement period (0.35→0.55) as the constraint has required active
 *   institutional enforcement (councils standardizing terminology) even while
 *   maintaining the fiction that standards simply crystallize from usage. The
 *   reading is a tangled rope: genuine coordination function (shared Latin
 *   enables church governance across regions) combined with asymmetric
 *   extraction (clerical and scholar practice becomes normative; other
 *   speaker populations are excluded; textual purists are progressively
 *   marginalized). This constraint is ONE reading of the contested kernel
 *   'latin_correctness'; the sibling readings 'textual_recovery_reading' and
 *   'hybrid_adequacy_reading' are separate constraint stories with different
 *   ε values, beneficiaries, and perspectives.
 *
 * KEY AGENTS:
 *   - Practicing Scholars and Clerics: Primary beneficiary (institutional/arbitrage) — their institutional practice defines correctness; can innovate and have innovations legitimated through usage. Net extractors from the constraint.
 *   - Textual Purists: Primary victim (powerless/trapped) — advocates for canonical classical forms; progressively marginalized as institutional practice becomes the standard. No alternative authority to validate their position; trapped by institutional dominance of living practice.
 *   - Ecclesiastical Authority: Secondary beneficiary and enforcer (organized/constrained) — coordinates via shared Latin; uses councils to standardize terminology. Benefits from stable communication but constrained by need to accommodate regional drift and new vocabulary.
 *   - Conservative Copyists: Secondary victim (moderate/constrained) — maintain classical forms while accommodating innovations; face pressure to innovate but lack explicit authorization. Experience extraction as constraint to maintain standards against lived practice.
 *   - Renaissance Humanists: Institutional actor (institutional/arbitrage) — claim to recover classical authority while instantiating living drift; maintain high theater by performing classical purity. Constrained minimally because they hold enough status to avoid accountability.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the institutional choice (privileging clerical practice) as a linguistic law. False summit candidate: the constraint appears to derive from linguistic necessity but actually derives from institutional power asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_drift_reading, 0.38).
domain_priors:suppression_score(living_drift_reading, 0.32).
domain_priors:theater_ratio(living_drift_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_drift_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(living_drift_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(living_drift_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_drift_reading, tangled_rope).
narrative_ontology:human_readable(living_drift_reading, "Living Drift Reading: Latin Correctness as Continuous Institutional Practice").
narrative_ontology:topic_domain(living_drift_reading, "historical_linguistics/philology/renaissance_studies").

domain_priors:requires_active_enforcement(living_drift_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_drift_reading, '1c48adb1-f5c0-4c1b-81c8-80f741e14826').
narrative_ontology:cs_created_at('1c48adb1-f5c0-4c1b-81c8-80f741e14826', '').
narrative_ontology:cs_kernel_codification('1c48adb1-f5c0-4c1b-81c8-80f741e14826', distributed).
narrative_ontology:cs_authority_grounding('1c48adb1-f5c0-4c1b-81c8-80f741e14826', lineage).
narrative_ontology:cs_interpretation_layer_present('1c48adb1-f5c0-4c1b-81c8-80f741e14826').
narrative_ontology:cs_kernel_id(living_drift_reading, latin_correctness).
narrative_ontology:cs_reading_relation('1c48adb1-f5c0-4c1b-81c8-80f741e14826', textual_recovery_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c48adb1-f5c0-4c1b-81c8-80f741e14826', hybrid_adequacy_reading, influences).
narrative_ontology:cs_axiom('1c48adb1-f5c0-4c1b-81c8-80f741e14826', foundational, usage_is_norma_loquendi).
narrative_ontology:cs_axiom_status(usage_is_norma_loquendi, holdable).
narrative_ontology:cs_axiom_grounding('1c48adb1-f5c0-4c1b-81c8-80f741e14826', usage_is_norma_loquendi, empirically_contingent).
narrative_ontology:cs_axiom('1c48adb1-f5c0-4c1b-81c8-80f741e14826', foundational, institutional_practice_constitutes_competence).
narrative_ontology:cs_axiom_status(institutional_practice_constitutes_competence, holdable).
narrative_ontology:cs_axiom_grounding('1c48adb1-f5c0-4c1b-81c8-80f741e14826', institutional_practice_constitutes_competence, conventional).
narrative_ontology:cs_reference_frame('1c48adb1-f5c0-4c1b-81c8-80f741e14826', medieval_clerical_practice_standard).
narrative_ontology:cs_drift_state('1c48adb1-f5c0-4c1b-81c8-80f741e14826', renaissance_humanist_challenge, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_drift_reading, practicing_scholars).
narrative_ontology:constraint_beneficiary(living_drift_reading, clerical_scribes).
narrative_ontology:constraint_beneficiary(living_drift_reading, institutional_continuity).
narrative_ontology:constraint_victim(living_drift_reading, textual_purists).
narrative_ontology:constraint_victim(living_drift_reading, canonical_reconstruction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUAL PURIST (SNARE) — Advocates for recovery of 'correct' classical Latin forms based on literary canon (Cicero, Virgil). Trapped by institutional dominance of living practice; no alternative authority to validate their position. Bears full cost of linguistic evolution: each generation's innovations render prior prescriptive standards obsolete. Cannot exit the constraint; extractive force runs continuously away from this agent.
constraint_indexing:constraint_classification(living_drift_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONSERVATIVE MONASTERY SCRIPTORIUM (TANGLED ROPE) — Institutional copyists who maintain classical forms while accommodating necessary innovations (new technical vocabulary for Christian concepts). Experience both coordination (copying standardized texts enables knowledge transmission) and extraction (pressure to innovate without explicit authorization). Constrained by resource scarcity and institutional hierarchy; moderate cost to maintaining classical standards against lived practice.
constraint_indexing:constraint_classification(living_drift_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRACTICING SCHOLAR-CLERIC (ROPE) — Benefits from allowing Latin to evolve with institutional needs: new vocabulary for theology, medicine, law; pronunciation aligned with vernacular speech for pedagogical efficiency; orthography reflecting actual usage. Experiences the constraint as coordination: living practice enables rapid knowledge transmission and institutional adaptation. Net beneficiary — they set standards through usage, and the constraint legitimates their innovations.
constraint_indexing:constraint_classification(living_drift_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ECCLESIASTICAL AUTHORITY (TANGLED ROPE) — Church institutions coordinate across dioceses via Latin but must accommodate regional vernacular drift and new theological terminology. Active enforcement of 'correctness' based on continuous practice (not classical canon) — requires councils to standardize terminology. Both coordination function (shared language enables church governance) and extraction (selective standardization privileges certain regional practices over others; excludes non-institutional speech communities). Moderate suppression; escape is constrained by institutional dependence on Latin.
constraint_indexing:constraint_classification(living_drift_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: RENAISSANCE HUMANIST AUTHORITY (PITON) — Post-medieval humanists claim to recover authentic classical Latin through textual criticism. Their authority derives from historical scholarship but their actual practice incorporates living innovations constantly. Theater ratio high: they perform classical purity while instantiating living drift. Theater has degraded because the recovered classical forms never fully displace institutional practice — both coexist in tension. Maintained through institutional inertia (humanist prestige) and aesthetic commitment despite functional obsolescence.
constraint_indexing:constraint_classification(living_drift_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, linguistic drift in living languages is structurally inevitable: all languages evolve continuously in speaker populations, and institutional practices crystallize the innovations that serve communication needs. This perspective sees 'correct Latin' as necessarily defined by usage (Horace: 'usus est norma loquendi'), making textual purism a futile resistance to linguistic natural law. However, beneficiary declarations reveal this as a false summit: the constraint naturalizes the institutional decision to accept clerical and scholar practice as authoritative while excluding other speaker populations.
constraint_indexing:constraint_classification(living_drift_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_drift_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(living_drift_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(living_drift_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(living_drift_reading, TR),
    TR >= 0.70.

:- end_tests(living_drift_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The living drift reading legitimates innovation while suppressing textual purist claims. The extraction is real — textual purists bear the cost of being marginalized — but not severe because the reading genuinely solves coordination problems (institutional practice enables rapid knowledge transmission). The measurement trajectory shows extractiveness rising from 0.20 to 0.40 over the interval, reflecting increasing institutional enforcement pressure as regional drift accelerated and clerical innovation became more explicit. Theater ratio (0.55): Moderate-high and rising. The constraint requires active institutional enforcement (councils, standardization efforts) even though the reading claims standards simply emerge from usage. The performance gap widens as regional variation increases — the fiction that practice crystallizes naturally becomes harder to maintain when councils must actively intervene. Rising theater suggests the coordination function is degrading relative to the performative maintenance required. Suppression (0.32): Moderate. Textual purists face institutional marginalization but are not physically prevented from advocating classical forms. Their suppression is primarily institutional (access to authority is restricted; their claims are delegitimized) rather than physical. However, the access restriction is real — without institutional recognition, textual purist arguments carry minimal weight. The measurement trajectory suggests suppression has stabilized: once the institutional norm crystallized in medieval practice, purist resistance faced consistent, predictable barriers rather than escalating coercion.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is severe and structures the entire constraint. From the practicing scholar's perspective (rope), the constraint solves a genuine problem: allowing Latin to evolve with institutional needs enables rapid theological development and maintains a shared lingua franca across regions. Their experience is coordination — the constraint legitimates their innovations, enabling institutional communication. From the textual purist's perspective (snare), the same constraint is extractive: their authority claims are systematically delegitimized, their preferred forms are progressively excluded from legitimate discourse, and they have no institutional mechanism to resist. The ecclesiastical authority occupies the center: they benefit from innovation (new theological vocabulary) but must enforce standards (through councils) to prevent complete fragmentation. The renaissance humanists occupy a piton perspective: they claim to recover classical authority but their actual practice drifts from classical models — the humanist texts themselves become a new norm that later humanists drift from. The conservation copyist occupies a constrained tangled rope: they experience both coordination (standardized copying enables knowledge transmission) and extraction (pressure to innovate without authorization; tension between classical forms and living practice). The analytical observer risks naturalizing the institutional choice as linguistic law, missing that the 'naturalness' of living drift derives from clerical and scholarly dominance, not from linguistic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. Practicing scholars and ecclesiastical authorities are beneficiaries with institutional power and exit options (arbitrage — they can shift what counts as 'correct' through institutional adoption); their d values are low (0.05–0.20), producing negative or near-zero effective extraction (χ). Textual purists are victims with powerless status and trapped exit options (no alternative institutional authority); their d value is high (0.95), producing maximum experienced extraction (χ). Conservative copyists are secondary victims with moderate power and constrained exit; their d value is moderate-high (0.65–0.75), producing substantial but not maximal extraction (χ). Renaissance humanists appear to have arbitrage-level exit (institutional prestige, ability to shift what counts as correct) but are trapped by identity fusion with the humanist project of classical recovery — their d should be elevated from the canonical arbitrage value (~0.15) to account for this partial identity lock, perhaps 0.30–0.40. The analytical observer has analytical exit and analyses power, placing them at a canonical d of ~0.73 (moderate experienced extraction due to the risk that natural law framing naturalizes institutional power).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying what 'correctness' means: under the living drift reading, correctness is definitional (the form speakers use), not evaluative (the form speakers should use). This dissolves the apparent contradiction between 'Latin must follow rules' (rule-based authority) and 'Latin is constantly evolving' (empirical observation). The living drift reading privileges the empirical observation and makes it into the rule: correctness IS the form speakers use. The textual recovery reading privileges rule-based authority and makes the empirical evolution into the problem: speakers are deviating from correct forms. The hybrid adequacy reading treats both as real and in tension. No reading is objectively correct — they are different authority frameworks that generate different classifications of the same phenomena. The constraint's classification (tangled rope) reflects this: it genuinely coordinates institutional communication (rope function) while suppressing alternative frameworks for defining correctness (snare function toward purists). Mandatrophy is resolved by recognizing that the constraint's type is stable given the chosen reading — it will always be tangled rope under living drift because the reading commits to both coordination (institutional practice enables communication) and extraction (suppression of alternatives). A different reading would produce a different type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_practice_definition_boundary,
    'What constitutes ''continuous institutional practice'' — which speaker populations count as competent, and what threshold of usage establishes a form as correct?',
    'Historical analysis of manuscript variation; identification of which speakers'' innovations became canonized vs. rejected; comparison of contemporary documentary evidence with later prescriptive judgments',
    'If boundary is narrow (only elite clerics count): living drift reading reduces to oligarchic extraction (higher chi). If boundary is broad (including lower clergy and lay scholars): genuine coordination function is robust (lower chi). Determines whether institutional practice is a coordination mechanism or a legitimized monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_practice_definition_boundary, empirical, 'Definition of competent speaker population in institutional practice').

omega_variable(
    kernel_reading_contest_this_reading_instantiates,
    'Is ''correct Latin'' the form that practicing speakers use (living drift reading), the form preserved in canonical texts (textual recovery reading), or a hybrid adequacy that balances both (hybrid adequacy reading)?',
    'This omega documents the kernel itself — the reading you are instantiating (living_drift_reading) is one resolution of this irreducible contest. The other readings (textual_recovery_reading, hybrid_adequacy_reading) are authored as separate constraint stories with their own ε values and perspectives.',
    'If living drift is correct: fountain of authority is current institutional practice, theater is high (purists perform rejection), extraction is moderate. If textual recovery is correct: fountain of authority is canonical texts, suppression of innovation is high, extraction is severe (purists have stronger position). If hybrid adequacy is correct: multiple fountains coexist in tension, extraction depends on which reading holds institutional power at a given moment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_this_reading_instantiates, conceptual, 'This constraint is one reading of the contested kernel: what defines correct Latin?').

omega_variable(
    suppression_mechanism_naturalization,
    'Is the suppression of textual purist claims a structural feature of linguistic reality or an institutional choice to privilege living practice over canonical authority?',
    'Historical examination of moments when purist challenges successfully reset institutional standards (e.g., humanist script reform, Erasmian pronunciation) vs. moments when living practice overwhelmed purist resistance; analysis of which suppression mechanisms are linguistic (communication efficiency, cognitive load) vs. institutional (status hierarchies, access to canonical texts)',
    'If suppression is largely linguistic (efficiency requires some standardization): constraint is closer to rope (coordination with natural friction). If suppression is largely institutional (clerical monopoly on text access and prestige): constraint is closer to snare from purist perspective. Determines whether the textual purist''s trapped status is a feature of linguistic reality or a feature of institutional power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_naturalization, empirical, 'Whether suppression of purist positions is linguistic or institutional').

omega_variable(
    medieval_to_renaissance_transition_drift_state,
    'Did the shift from medieval living Latin to Renaissance humanist revival represent a genuine recovery of classical standards, or did humanists instantiate living drift while claiming textual authority?',
    'Linguistic analysis of humanist texts vs. stated classical models; identification of innovations humanists introduced while asserting classical purity; examination of whether later generations of humanists similarly drifted from humanist models',
    'If genuine recovery: textual recovery reading is viable and living drift is deviation from a stable classical baseline. If humanists drifted: the living drift reading reveals that Renaissance humanists were doing exactly what they accused medieval clerics of doing — evolving Latin to institutional needs while claiming canonical authority. The piton perspective''s degraded theater becomes historically cyclical: each generation claims classical purity while innovating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_to_renaissance_transition_drift_state, empirical, 'Whether Renaissance recovery was genuine or itself an instance of living drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_drift_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(living_drift_tr_t0, living_drift_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(living_drift_tr_t3, living_drift_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(living_drift_tr_t6, living_drift_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(living_drift_tr_t9, living_drift_reading, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(living_drift_be_t0, living_drift_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(living_drift_be_t3, living_drift_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(living_drift_be_t6, living_drift_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(living_drift_be_t9, living_drift_reading, base_extractiveness, 9, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_drift_reading, information_standard).
narrative_ontology:affects_constraint(living_drift_reading, textual_recovery_reading).
narrative_ontology:affects_constraint(living_drift_reading, hybrid_adequacy_reading).

% DUAL FORMULATION NOTE:
% The kernel 'latin_correctness' decomposes into three constraint stories, each with a different ε value and institutional perspective. The living_drift_reading (ε=0.38) treats usage as the fountain of authority. The textual_recovery_reading (ε≈0.55–0.65, expected) treats canonical texts as the fountain. The hybrid_adequacy_reading (ε≈0.40–0.50, expected) treats both as legitimate in tension. Each story has its own beneficiaries, victims, and measurement trajectories. They affect each other: the rise of humanist textual authority constrains living drift reading's institutional dominance; conversely, institutional need for innovation maintains living drift despite humanist challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_drift_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
