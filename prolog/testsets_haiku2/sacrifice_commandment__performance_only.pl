% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Performance-Only Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice commandment kernel declares
 *   that the sacrificial law requires physical execution within a functioning
 *   Temple. Without the Temple, the commandment is suspended (halakhically
 *   inactive), not fulfilled by study, archival preservation, or
 *   substitution. This reading has been institutionally dominant in Talmudic
 *   scholarship since the early medieval period. The constraint operates as a
 *   tangled rope: it coordinates the preservation of legal knowledge (genuine
 *   coordination function) while extracting 1,900 years of devoted scholarly
 *   attention toward study of unperformable law (asymmetric extraction from
 *   observant communities). Suppression is maintained through institutional
 *   authority: alternative readings (study-as-performance,
 *   archive-maintenance) are excluded from the authorized interpretive
 *   framework, and observant communities are identity-locked into acceptance
 *   of the suspension. Theater has risen over time as the performative
 *   function of study has intensified—contemporary scholarship spends
 *   substantial effort justifying the study of suspended commandments rather
 *   than focusing on how the knowledge might be applied to living law. The
 *   claim/metric gap is structural: the reading claims authentic halakhic
 *   transmission while the metrics describe highly extractive institutional
 *   enforcement of a contestable interpretation.
 *
 * KEY AGENTS:
 *   - Talmudic scholarship institution: Institutional agenda-setter that maintains and enforces the performance-only reading; benefits from the sustained authority granted by the commandment suspension framework.
 *   - Observant Jewish communities: Organized payers; identity-locked into the tradition; bear the cost of suspended commandment status without access to alternative resolutions.
 *   - Messianic restoration seekers: Moderate power, civilizational time horizon; locked into waiting for Temple restoration; excluded from present-age alternatives.
 *   - Study-as-performance advocates: Moderate power, mobile exit; propose intellectual engagement counts as commandment performance; excluded from institutional authority.
 *   - Archive-maintenance readers: Moderate power, mobile exit; frame study as preservation for future restoration, not commandment fulfillment; excluded from institutional framework.
 *   - Interpretation authority council: Analytical observer; can assess whether performance-only is mandatory or contested.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.81).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.72).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.81).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '674e1e04-3342-4389-b174-8f0453de824f').
narrative_ontology:cs_kernel_codification('674e1e04-3342-4389-b174-8f0453de824f', fixed_text).
narrative_ontology:cs_authority_grounding('674e1e04-3342-4389-b174-8f0453de824f', lineage).
narrative_ontology:cs_interpretation_layer_present('674e1e04-3342-4389-b174-8f0453de824f').
narrative_ontology:cs_reading_relation('674e1e04-3342-4389-b174-8f0453de824f', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('674e1e04-3342-4389-b174-8f0453de824f', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('674e1e04-3342-4389-b174-8f0453de824f', foundational, physical_execution_required).
narrative_ontology:cs_axiom_status(physical_execution_required, holdable).
narrative_ontology:cs_axiom_grounding('674e1e04-3342-4389-b174-8f0453de824f', physical_execution_required, deontological).
narrative_ontology:cs_axiom('674e1e04-3342-4389-b174-8f0453de824f', foundational, study_is_suspension_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_suspension_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('674e1e04-3342-4389-b174-8f0453de824f', study_is_suspension_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('674e1e04-3342-4389-b174-8f0453de824f', temple_destruction_legal_framework).
narrative_ontology:cs_drift_state('674e1e04-3342-4389-b174-8f0453de824f', contemporary_diaspora_permanent_condition, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('674e1e04-3342-4389-b174-8f0453de824f', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, talmudic_scholarship_institution).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, jewish_observant_communities).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, messianic_restoration_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and transmits Talmudic legal frameworks that declare the sacrifice commandment suspended (not fulfilled) in the absence of the Temple. Controls interpretive authority over what constitutes fulfillment of divine law. Benefits from sustained scholarly attention to sacrificial detail and halakhic machinery, as this study maintains institutional legitimacy and centrality within Jewish religious life. The institutional apparatus requires complex textual engagement with unperformable acts to justify its interpretive role.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_scholarship_institution, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Bound by halakhic obligation to honor the commandment structure but prohibited from physical performance. The constraint directs their devotional energy toward study of unperformable law rather than toward alternative commandments that could be executed in the present. They carry identity as observant Jews whose covenant requires submission to this interpretive framework, making exit costly. The performance-only reading leaves them in a permanent state of commandment suspension, not suspension bridged by permissible alternatives.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, jewish_observant_communities, payer,
    organized, generational, identity_locked, global).

% Await restoration of the Temple as a precondition for fulfillment of the commandment. The performance-only reading suspends the entire category of sacrificial obligation until that restoration occurs, which may not occur. They are excluded from alternative framings (study-as-performance) that might resolve the suspension in the present. Identity as members of a tradition awaiting redemption locks them into acceptance of the suspended state.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_restoration_seekers, payer,
    moderate, civilizational, identity_locked, national).

% Propose that intellectual engagement with sacrificial law constitutes performance of the commandment in the present age. They are excluded from the performance-only framework's authority structure and cannot present their reading as legitimate within institutional Talmudic channels. Their alternative would redirect scholarly labor toward different interpretive ends.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, study_as_performance_advocates, excluded,
    moderate, generational, mobile, regional).

% Frame study of sacrificial law as preservation of technical knowledge for future Temple restoration, without claiming that study itself fulfills the commandment. They propose a different justification for the same scholarly labor but are excluded from the performance-only reading's warrant. Their reading would license the study while decoupling it from commandment fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, archive_maintenance_readers, excluded,
    moderate, civilizational, mobile, regional).

% Comparative analytical observer across Jewish jurisprudence: can evaluate whether the performance-only reading is mandatory, normative, or contested; can assess the structural consequences of each reading for observant communities and scholarly institutions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, interpretation_authority_council, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, talmudic_scholarship_institution).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of sacrificial law across 1,900 years of diaspora through organized Talmudic study, maintaining textual precision and interpretive continuity despite the absence of live performance. Ensures that if the Temple is restored, the knowledge base is complete and available.
% TRANSFER_FUNCTION: Transfers scholarly attention and interpretive authority from living commandments (those performable in diaspora) to detailed study of suspended commandments (unperformable sacrifice law). Directs observant communities' devotional engagement toward study of law without the reward of fulfillment. Sustains institutional authority by maintaining complexity of the interpretive apparatus.
% ABSENT_VOICES: Study-as-performance advocates and archive-maintenance readers are excluded from the institutional authority structure that declares the performance-only reading binding. They would argue that the constraint's suspension is unnecessary and misdirects scholarly labor. Broader Jewish communities in diaspora who might prefer alternative frameworks for relating to suspended commandments have no formal seat.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared and were replaced by study-as-performance or archive-maintenance framings, the entire justificatory structure for sacrificial study would shift. Observant communities could reorient devotional energy toward alternative interpretive claims. The institutional authority structure defending the suspension would lose its principal warrant. Scholarly labor would either redirect toward different ends or be reframed as preparation rather than suspension.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the sacrificial commandments became literally unperformable. The Jewish legal tradition required a framework for relating to the destroyed commandment structure. The performance-only reading declared the commandments suspended, not fulfilled by substitute acts, and mandated preservation of the technical knowledge through study.
% FOUNDING_PROBLEM_CORROBORATION: The Talmudic scholarship institution attests that the founding problem is still live and that the performance-only reading is the authentic transmission of Halakhic principle. Study-as-performance and archive-maintenance advocates, as well as contemporary Jewish philosophers and denominational leaders outside the ultra-Orthodox interpretive monopoly, contest this framing. Textual-historical scholars note that multiple readings co-existed in medieval and early modern sources; the performance-only reading became institutionally dominant through authority consolidation, not unanimous acceptance.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 at T0 (early post-Temple period, the reading was newly necessary and less extractive because Temple restoration seemed possible) to 0.81 at T1900 (contemporary period, 1,900 years of non-restoration have made the suspension permanent-seeming, yet the study obligation persists unchanged). Theater rises from 0.35 to 0.68: early on, the study of sacrificial law was justified as preparation and legal preservation. Contemporarily, the institutional apparatus spends substantial effort justifying why the study continues despite permanent suspension—this justification is performative, not functionally tied to live obligation. Suppression rises from 0.48 to 0.72 as institutional mechanisms harden: medieval sources show competing readings debated openly; by modernity, the performance-only reading is declared settled, and alternative frameworks are marginalized. The measurement series tracks one shared time grid (every metric at every time point) showing accumulation of extraction and theater, with suppression hardening in parallel. The manuscript and transmission history supports the observed growth: early Amoraic sources (T0–200) preserved multiple warrants; Geonic period (T200–600) saw consolidation; medieval Tosafist tradition (T600–1500) began systematic suppression of alternatives; modernity (T1500–1900) witnesses near-universal institutional alignment.
 *
 * PERSPECTIVAL GAP:
 *   From the Talmudic scholarship seat, the performance-only reading is the authentic halakhic transmission: a faithful preservation of commandment structure in the absence of Temple, justified by law and tradition. From the observant community seat, the same structure appears as suspended commandment with no resolution path, extracting their devotional energy toward study without fulfillment. From the study-as-performance seat, the reading is unnecessarily restrictive and misdirects scholarly labor. From the archive-maintenance seat, the reading conflates preparation (justified) with commandment fulfillment (a separate claim not made). The engine will compute different classifications from each seat: the institutional seat may classify the constraint as coordinating preservation; the observant community seats will classify it as extractive and suppressive. This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Talmudic scholarship institution is a clear beneficiary (d near 0.0): it accrues interpretive authority, institutional centrality, and sustained scholarly labor from the suspension framework. Observant communities are targets (d near 1.0): they are bound by halakhic obligation to study and honor a suspended commandment, with no performative outlet and constrained exit (identity-locked). Messianic seekers are also targets: they await restoration they cannot hasten, suspended in a state they did not choose. Excluded alternatives (study-as-performance, archive-maintenance) would reduce extraction if admitted to the authority framework. The institutional power asymmetry is the structural source: one institutional apparatus (Talmudic scholarship) controls the interpretive warrant and can exclude competing readings. Communities and individuals have organized power but are identity-locked into accepting the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction, need for legal framework) was live and urgent at T0. By T1900, the founding problem's status is contested: the Talmudic institution claims the problem persists (Temple not restored, knowledge must be preserved); observant communities and alternative readings claim the founding problem has been solved by accepting a permanently diaspora-based Judaism or by reframing study-as-performance. The performance-only reading persists because institutional authority enforces it, not because the founding problem drives new commitment. This is a candidate for mandatrophy: the original warrant (we must preserve knowledge in case of Temple restoration) has atrophied into institutional theater (we study because we study), yet the extraction apparatus remains. Theater ratio rising from 0.35 to 0.68 confirms the atrophy diagnosis: the performative justification for study has grown while the functional warrant has dimmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temple_restoration_probability,
    'What is the probability that the Temple will be physically restored, and over what timeframe? Does the performance-only reading presume a specific restoration date or probability?',
    'Examination of classical and medieval Talmudic sources for explicit restoration timelines or implicit probability assumptions. Comparison with contemporary messianic thought.',
    'If the reading presumes imminent restoration, the suspension is temporary and the extraction is justified. If restoration is indefinite or probabilistically low, the suspension becomes permanent extraction masquerading as temporary. High probability/imminent timeline → reduces extractiveness claim. Low probability/indefinite timeline → increases extractiveness (the suspension will likely never lift).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_probability, empirical, 'Whether the suspension presumes eventual Temple restoration or permanent diaspora condition.').

omega_variable(
    study_as_alternative_fulfillment,
    'Is intellectual engagement with sacrificial law capable of constituting commandment performance, or is performance fundamentally bounded to physical execution? Is this a halakhic question (disputable within tradition) or a metaphysical necessity?',
    'Textual-historical analysis of medieval and early modern sources that debate or accept study-as-performance alternatives. Examination of whether the performance-only reading is declared settled by decisive halakhic reasoning or by institutional authority consolidation.',
    'If study-as-performance is halakhically viable (disputed, not refuted), the performance-only reading is one contestable option and the exclusion of alternatives is institutional suppression, not legal certainty. This would reclassify the constraint from institutionally justified suspension to institutionally enforced extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_alternative_fulfillment, conceptual, 'Whether the performance-only reading is halakhically mandatory or institutionally chosen.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) structural (external exclusion of alternative readings from authority forums) or internalized (observant communities have internalized the performance-only reading as the authentic tradition and would not adopt alternatives even if exposed to them)?',
    'Historical studies of communities that encountered study-as-performance or archive-maintenance framings: did they actively reject the alternatives or adopt them when institutional constraints were removed? Post-denominal shifts in Jewish communities that have adopted different readings.',
    'If suppression is mostly structural, removing institutional barriers would shift the constraint significantly. If internalized, the suppression persists even after institutional exclusion is lifted. The constraint''s effective suppression could be higher than the structural measure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of alternatives is structural or internalized in observant communities.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the performance-only reading''s core premise (physical execution required) foreclose the study-as-performance reading (intellectual engagement fulfills), or can both coexist in different institutional frameworks?',
    'Logical analysis of the foundational axioms: if the performance-only reading holds that ''study is not performance,'' then study-as-performance cannot be true within that framework. But can both readings coexist across different communities and still be considered part of the same tradition?',
    'If foreclosure applies (one reading logically rules out the other in a single framework), the relationship is coexists_with (different communities hold different readings, neither logically eliminates the other). If the readings are logically incompatible, the relationship would be forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether performance-only and study-as-performance logically foreclose each other or coexist across different authority structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_commandment__performance_only, theater_ratio, 200, 0.41).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_commandment__performance_only, theater_ratio, 600, 0.52).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.61).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.66).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.68).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sacr_be_t200, sacrifice_commandment__performance_only, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(sacr_be_t600, sacrifice_commandment__performance_only, base_extractiveness, 600, 0.68).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.76).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.79).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sacr_su_t200, sacrifice_commandment__performance_only, suppression_requirement, 200, 0.55).
narrative_ontology:measurement(sacr_su_t600, sacrifice_commandment__performance_only, suppression_requirement, 600, 0.62).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.68).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.71).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.12).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints, each with a different reading of what constitutes honoring the sacrificial law absent the Temple. This constraint (performance_only) claims physical execution is required and study is suspension, not fulfillment. Sibling constraints (study_as_performance, archive_maintenance) claim study itself is the performance or that study is justified as preservation for future restoration. Each reading instantiates a different ε (this one high-extractive at 0.81; study-as-performance would be near-zero; archive-maintenance would be moderate). The readings have different victim sets and institutional consequences. They are linked via network.affects_constraints to show family kinship. Each story is internally ε-invariant; the family structure shows how a single halakhic kernel generates multiple structurally distinct constraints when different communities read it differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
