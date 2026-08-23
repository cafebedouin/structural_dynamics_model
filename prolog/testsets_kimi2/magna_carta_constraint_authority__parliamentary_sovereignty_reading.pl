% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Constraint Authority (Parliamentary Sovereignty Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   Under the parliamentary sovereignty reading, Magna Carta's historic
 *   constraints on the Crown survive only as absorbed into Acts of
 *   Parliament. Parliamentâspecifically legislative majoritiesâinherits
 *   the authority to enforce, revise, or repeal any charter provision. The
 *   constraint appears as a coordination mechanism (translating medieval
 *   liberties into modern statute) but operates with asymmetric extraction:
 *   minorities lose entrenched protection and become vulnerable to
 *   majoritarian repeal, while the Crown's prerogative remains bounded by
 *   parliamentary rather than independent charter authority. The claim/metric
 *   independence is maintained: the constraint is claimed as tangled_rope
 *   while metrics are authored descriptively.
 *
 * KEY AGENTS:
 *   - parliamentary_majorities (agenda_setter/beneficiary): Controls the statute book and can revise or repeal Magna Carta provisions by ordinary majority
 *   - minority_groups (payer): Unprotected by entrenched restraints, vulnerable to majoritarian repeal of statutory safeguards
 *   - crown_executive (payer): Prerogative constrained, cannot override parliamentary statute
 *   - judiciary (observer): Interprets statutory derivatives but cannot bind Parliament
 *   - common_law_constitutionalists (excluded): Argue for binding fundamental law, structurally excluded from the parliamentary sovereignty framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.55).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Constraint Authority (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '77d3c411-65f7-4970-b7fd-1ca5794b9102').
narrative_ontology:cs_kernel_codification('77d3c411-65f7-4970-b7fd-1ca5794b9102', fixed_text).
narrative_ontology:cs_authority_grounding('77d3c411-65f7-4970-b7fd-1ca5794b9102', lineage).
narrative_ontology:cs_interpretation_layer_present('77d3c411-65f7-4970-b7fd-1ca5794b9102').
narrative_ontology:cs_reading_relation('77d3c411-65f7-4970-b7fd-1ca5794b9102', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('77d3c411-65f7-4970-b7fd-1ca5794b9102', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('77d3c411-65f7-4970-b7fd-1ca5794b9102', foundational, parliament_may_revise_any_charter_provision).
narrative_ontology:cs_axiom_status(parliament_may_revise_any_charter_provision, holdable).
narrative_ontology:cs_axiom_grounding('77d3c411-65f7-4970-b7fd-1ca5794b9102', parliament_may_revise_any_charter_provision, conventional).
narrative_ontology:cs_reference_frame('77d3c411-65f7-4970-b7fd-1ca5794b9102', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('77d3c411-65f7-4970-b7fd-1ca5794b9102', contemporary_constitutional_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('77d3c411-65f7-4970-b7fd-1ca5794b9102', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and control Magna Carta's constraint authority through statute law; can revise or repeal any charter provision by ordinary legislative majority, operating as both the administrator and primary beneficiary of the flexible arrangement.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities, beneficiary).

% Bear the risk that parliamentary majorities will repeal statutory protections derived from Magna Carta; lack veto or entrenchment mechanisms to prevent majoritarian revision of their safeguards.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, generational, trapped, national).

% Royal prerogative is constrained by Magna Carta provisions that survive only as parliamentary statute; the Crown cannot invoke the charter independently against Parliament and must operate within statutory limits.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive, payer,
    powerful, biographical, constrained, national).

% Recognizes Magna Carta's provisions only insofar as they have been incorporated into statute; cannot enforce the charter against Parliament and treats parliamentary sovereignty as the foundational rule of recognition.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Maintain that Magna Carta embodies fundamental law binding all branches including Parliament; this reading excludes their position by treating charter authority as entirely derivative of and subordinate to parliamentary statute.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, common_law_constitutionalists, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mediates historical restraints on executive and royal power through a democratic legislature, translating feudal charter limitations into modern statutory form subject to contemporary democratic revision.
% TRANSFER_FUNCTION: Transfers constraint authority from the Crown and medieval charter text to Parliament as sovereign legislature; transfers risk of protection-loss from the charter's fixed text to the political process where majorities control outcomes.
% ABSENT_VOICES: Common law constitutionalists who argue for entrenched fundamental law, and minority communities whose protections depend on entrenched rather than majoritarian guarantees, are structurally absent from the parliamentary sovereignty framework.
% DISAPPEARANCE_RATIONALE: If the parliamentary sovereignty reading vanished, statutory incarnations of Magna Carta would require alternative authority grounding such as common law entrenchment, popular constitutionalism, or supra-legislative norms, shifting power from legislative majorities to courts or popular assemblies; Crown prerogative boundaries and minority protections would rest on a different constitutional logic.
% FOUNDING_PROBLEM: Medieval royal absolutism and arbitrary exercise of Crown prerogative without consultative or legal restraint; the barons sought to bind the King to lawful process.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and legal scholars outside the beneficiary set attest that arbitrary royal power was the 13th-century problem. Contemporary political theorists and human rights monitors outside the parliamentary-majority beneficiary set attest that the current problem has shifted to majoritarian override of minority protections.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint genuinely coordinates historical liberties into modern law but extracts from minorities and the Crown by subordinating their protections to majoritarian will. Suppression (0.48) is moderate: alternatives such as entrenched constitutional review and common law fundamental rights persist in discourse but are structurally excluded from legal effect. Theater ratio (0.42) reflects moderate ceremonial veneration of Magna Carta alongside its real statutory function. Accessibility collapse (0.58) is moderate-to-high because once parliamentary sovereignty is accepted as the rule of recognition, higher-law alternatives collapse for legal professionals. Resistance (0.44) is moderate from common law constitutionalists and human rights advocates who contest the majoritarian premise.
 *
 * PERSPECTIVAL GAP:
 *   Parliamentary majorities experience the constraint as a flexible coordination tool preserving historic liberties subject to democratic revision. Minorities and the Crown experience it as a mechanism that removes fixed legal protections and subjects them to legislative caprice. The judiciary experiences it as a boundary on their own authority to enforce fundamental law. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majorities are the structural beneficiary (low d, near 0.0): they control the constraint and can dissolve it at will. Minority groups are the primary target (high d, near 1.0): they bear the risk of protection repeal with no structural veto. The Crown is a secondary target (moderate-high d): its prerogative is constrained by statute it cannot override. The judiciary sits near symmetric (d ~0.5): bound by the doctrine but also empowered to interpret statutory derivatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by acknowledging the genuine coordination function (translating medieval restraints into modern democratic statute) while insisting on the asymmetric extraction (majoritarian control over minority protections). Without the coordination component, this would be a snare; without the extraction component, it would be a rope. The tangled_rope classification captures both. The founding problem (restraining medieval royal absolutism) is dead, but the arrangement persists with a new function (mediating majoritarian sovereignty), which would flag piton risk absent the ongoing coordination role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_revision_risk,
    'Does the practical incidence of majoritarian repeal of Magna Carta-derived protections match the theoretical vulnerability asserted by the parliamentary sovereignty reading?',
    'Historical statutory analysis tracking how often Parliament has repealed or diluted Magna Carta-derived statutory protections, compared to the baseline of ordinary statute revision.',
    'If repeal is empirically rare, the effective extractiveness may be lower than modeled; if repeal is common or accelerating, the extraction metric is understated and the constraint trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_revision_risk, empirical, 'Whether majoritarian repeal of protections is frequent enough to justify the extraction score').

omega_variable(
    common_law_alternative_viability,
    'Could the UK legal system functionally shift to a common law constitutionalism reading without institutional collapse?',
    'Comparative constitutional analysis and examination of judicial dicta (Factortame, Miller, Thoburn) for recognition of fundamental law limits on parliamentary sovereignty.',
    'If a common law alternative is structurally viable, the accessibility_collapse metric may be overstated; if parliamentary sovereignty is the only workable rule of recognition, the constraint is more deeply entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_alternative_viability, conceptual, 'Whether common law constitutionalism is a live structural alternative').

omega_variable(
    parliamentary_sovereignty_reading_kernel_location,
    'How does the parliamentary sovereignty reading''s structural claim (Magna Carta binds only as parliamentary statute) differ from the living constitutionalism reading''s claim (Magna Carta binds all rulers through precedent)?',
    'Jurisprudential analysis of UK constitutional case law to determine whether courts treat Magna Carta-derived statutes as specially entrenched or ordinarily revisable.',
    'If courts recognize special entrenchment, the parliamentary sovereignty reading''s epsilon is overstated and directionality shifts toward symmetric; if courts affirm ordinary revisability, the reading is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_reading_kernel_location, conceptual, 'Structural disagreement location within the Magna Carta authority kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(magn_tr_t45, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement(magn_tr_t90, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement(magn_tr_t135, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 135, 0.45).
narrative_ontology:measurement(magn_tr_t180, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 180, 0.5).
narrative_ontology:measurement(magn_tr_t225, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 225, 0.55).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(magn_be_t45, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 45, 0.42).
narrative_ontology:measurement(magn_be_t90, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement(magn_be_t135, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 135, 0.58).
narrative_ontology:measurement(magn_be_t180, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 180, 0.5).
narrative_ontology:measurement(magn_be_t225, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 225, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(magn_su_t45, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement(magn_su_t90, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 90, 0.48).
narrative_ontology:measurement(magn_su_t135, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 135, 0.52).
narrative_ontology:measurement(magn_su_t180, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 180, 0.45).
narrative_ontology:measurement(magn_su_t225, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 225, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the magna_carta_constraint_authority kernel. The parliamentary sovereignty reading instantiates a tangled_rope with moderate extraction (Parliament controls revision, minorities bear risk). Sibling readings instantiate different structural types: living constitutionalism (likely rope or mountain depending on enforcement) and feudal obsolescence (likely piton or dead constraint). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
