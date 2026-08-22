% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority - Strict Orthodox Reading
 *   domain: systematic_theology/ecclesiology
 *
 * SUMMARY:
 *   The Nicene Creed, as read through the strict orthodox frame, functions
 *   not merely as a liturgical text but as a binding metaphysical contract
 *   enforced by a hierarchical clerical class. This reading instantiates one
 *   commitment-system constraint within the contested kernel of Nicene
 *   authority: it treats the creed's ontological claims as non-negotiable
 *   divine truths, and it channels institutional power toward policing
 *   cognitive adherence. The kernel's sibling
 *   readingsâsymbolic-confessional and liturgical-habituationâare
 *   structurally distinct constraints and are excluded from this
 *   classification. The authored metrics describe a constraint whose
 *   coordination component (shared ecclesial identity across empire and eras)
 *   is coupled to an extractive enforcement apparatus that sanctifies
 *   clerical monopoly and punishes heterodox communities and unauthorized lay
 *   interpretation.
 *
 * KEY AGENTS:
 *   - hierarchical_clergy (agenda_setter/institutional/global): administers doctrinal boundaries and captures interpretive authority
 *   - heterodox_communities (payer/moderate/regional): bear sanctions for theological deviation
 *   - lay_interpreters (payer/powerless/local): subordinated to clerical exegesis, subject to identity-locked exit
 *   - historical_critical_scholars (observer/analytical): analytical seat outside the benefiting structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.72).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.78).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority - Strict Orthodox Reading").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '02e676b7-5839-43ef-a015-daffdfcd5707').
narrative_ontology:cs_kernel_codification('02e676b7-5839-43ef-a015-daffdfcd5707', fixed_text).
narrative_ontology:cs_authority_grounding('02e676b7-5839-43ef-a015-daffdfcd5707', lineage).
narrative_ontology:cs_interpretation_layer_present('02e676b7-5839-43ef-a015-daffdfcd5707').
narrative_ontology:cs_reading_relation('02e676b7-5839-43ef-a015-daffdfcd5707', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('02e676b7-5839-43ef-a015-daffdfcd5707', nicene_creed_authority__liturgical_habituation_reading, forecloses).
narrative_ontology:cs_axiom('02e676b7-5839-43ef-a015-daffdfcd5707', foundational, creed_metaphysically_binding).
narrative_ontology:cs_axiom_status(creed_metaphysically_binding, holdable).
narrative_ontology:cs_axiom_grounding('02e676b7-5839-43ef-a015-daffdfcd5707', creed_metaphysically_binding, theological).
narrative_ontology:cs_axiom('02e676b7-5839-43ef-a015-daffdfcd5707', foundational, deviation_is_heresy_warranting_sanction).
narrative_ontology:cs_axiom_status(deviation_is_heresy_warranting_sanction, holdable).
narrative_ontology:cs_axiom_grounding('02e676b7-5839-43ef-a015-daffdfcd5707', deviation_is_heresy_warranting_sanction, theological).
narrative_ontology:cs_reference_frame('02e676b7-5839-43ef-a015-daffdfcd5707', divinely_revealed_orthodoxy).
narrative_ontology:cs_drift_state('02e676b7-5839-43ef-a015-daffdfcd5707', post_enlightenment_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('02e676b7-5839-43ef-a015-daffdfcd5707', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the creed as binding divine metaphysical law, adjudicates orthodoxy through magisterial authority, and enforces sanctions against deviation. Derives institutional legitimacy, social control, and epistemic monopoly from this role.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, beneficiary).

% Maintain dissenting theological positions regarding the creed's metaphysical claims. Subject to excommunication, censorship, or state-sanctioned violence. Must hide beliefs, flee, or accept social exclusion.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    moderate, biographical, trapped, regional).

% Individual believers who privately interpret scripture or metaphysical claims. Their interpretations are subordinated to clerical authority; public deviation risks social death within the faith community and formal spiritual sanctions.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, identity_locked, local).

% Academic observers who study the creed's historical contingency, textual development, and political enforcement without institutional stake in its authority.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historical_critical_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified metaphysical ontology and communal identity across geographically and culturally dispersed Christian populations under a single doctrinal standard.
% TRANSFER_FUNCTION: Moves interpretive authority, social legitimacy, and epistemic autonomy from lay believers and dissenting communities to the hierarchical clergy and orthodox institutions, enforced through spiritual and social sanctions.
% ABSENT_VOICES: Historical-critical scholars, non-Nicene Christian traditions, and lay theologians who read the creed as historically contingent or politically constructed are structurally excluded from authoritative interpretation and from the community's epistemic table.
% DISAPPEARANCE_RATIONALE: If the creed's binding metaphysical authority vanished, local interpretive diversity would immediately resurface, hierarchical clerical authority would lose its primary doctrinal monopoly, and the community would splinter into competing theological frameworksâthe institutional and social order depends on this constraint.
% FOUNDING_PROBLEM: Fourth-century theological fragmentation, imperial political instability, and competing Christologies threatened ecclesial unity and imperial coherence; the creed was constructed to codify a single metaphysical position.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchical clergy and imperial historians attest the problem was fragmentation. Historical-critical scholars and heterodox communities attest the problem was politically manufactured to consolidate episcopal and imperial power. External corroboration is split, with no non-interested party available.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers interpretive autonomy from individual believers to a clerical hierarchy under threat of sanction. Suppression (0.78) reflects the active enforcement required to maintain a single metaphysical ontology across linguistically and culturally diverse populations. Theater_ratio (0.45) captures the performative dimension of orthodoxyâpublic confession independent of private assentâwhich increases when enforcement outpaces genuine consensus. Accessibility_collapse (0.75) is high because once the creed is accepted as divine law, alternative ontologies become unthinkable within the community. Resistance (0.60) registers the persistent recurrence of heterodox movements and lay dissent despite sanctions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (hierarchical clergy) experiences the constraint as sacred stewardship and necessary guardianship of revelation; the engine will compute a low directionality for this seat. The payer seats (heterodox communities, lay interpreters) experience the same structure as coercive epistemic extraction; the engine will compute high directionality. The resulting per-seat classification divergenceâlikely tangled_rope or snare from the payer view versus rope-like coordination from the agenda-setter viewâis the signal the corpus is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The clergy are declared beneficiaries because they monopolize the power to adjudicate orthodoxy and derive institutional legitimacy from that monopoly. Heterodox communities and lay interpreters are declared victims because they bear the costs of exclusion, sanction, and lost autonomy. The clergy's exit is constrained by institutional identity; the victims' exit ranges from trapped (heterodox communities under active persecution) to identity_locked (lay interpreters for whom leaving means social death).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfourth-century theological fragmentationâwas arguably live at Nicaea but is now contested. The strict orthodox reading resists mandatrophy resolution by reactivating the founding crisis in every era (heresy as ever-present threat). The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags that the arrangement persists beyond its original solving context, sustained by institutional extraction rather than ongoing coordination need alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creed_natural_law_ambiguity,
    'Is the creed''s metaphysical ontology a discovered natural law of divine reality, or a constructed conciliar settlement enforced by institutional power?',
    'Historical analysis of conciliar politics, textual variation in early manuscripts, and sociological study of doctrinal enforcement versus spontaneous belief formation.',
    'If constructed, the constraint is a tangled rope or snare naturalized as mountain; if genuinely revealed law, it approaches mountain status for believers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creed_natural_law_ambiguity, conceptual, 'Natural law versus constructed authority ambiguity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of deviation structural (excommunication, censorship, state violence) or internalized (believers self-police due to theological identity fusion)?',
    'Post-exit trajectory study: do dissenters continue to self-sanction after institutional departure?',
    'If internalized, effective suppression exceeds structural measures, amplifying extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_structural_delta,
    'How would classification change if the symbolic confessional or liturgical habituation reading were adopted instead of the strict orthodox reading?',
    'Comparative classification across the constraint family.',
    'Symbolic confessional reading would likely register lower extraction and suppression; liturgical habituation reading would shift victim profiles from cognitive dissenters to non-participants in ritual performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Sibling reading structural delta').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_strict_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nicene_strict_tr_t10, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(nicene_strict_tr_t20, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(nicene_strict_tr_t30, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(nicene_strict_tr_t40, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(nicene_strict_tr_t50, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(nicene_strict_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nicene_strict_be_t10, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(nicene_strict_be_t20, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(nicene_strict_be_t30, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(nicene_strict_be_t40, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(nicene_strict_be_t50, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nicene_strict_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(nicene_strict_su_t10, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(nicene_strict_su_t20, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(nicene_strict_su_t30, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(nicene_strict_su_t40, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(nicene_strict_su_t50, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Nicene Creed authority kernel. The strict orthodox reading treats the creed as metaphysically binding with sanctions; the symbolic confessional reading treats it as historically contingent witness; the liturgical habituation reading treats it as identity performance independent of assent. These are separate constraints linked by shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
