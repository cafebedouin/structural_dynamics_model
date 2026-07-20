% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading of Vedic-Dharmic Corpus
 *   domain: religious/social/interpretive
 *
 * SUMMARY:
 *   This constraint is the bhakti_devotional_reading of the
 *   vedic_dharmic_corpus kernel. It instantiates the interpretive claim that
 *   sincere devotion (bhakti) to the divine supersedes birth-based caste
 *   status in determining spiritual authority and access. Historically
 *   articulated by poet-saints across the Indian subcontinent, the reading
 *   opens a parallel legitimating pathway that bypasses Brahmin hereditary
 *   monopoly without fully dismantling the underlying social hierarchy.
 *   Sibling readings include the hereditary_monopoly_reading (varna as
 *   divinely ordained and textually prescribed) and the
 *   reformist_egalitarian_reading (constitutional equality overrides
 *   traditional authority). The authored metrics are independent of the
 *   claimed rope classification: the reading is claimed as coordinating
 *   spiritual access, but the metrics acknowledge moderate residual
 *   extraction because caste hierarchy persists in social practice around the
 *   devotional ideal.
 *
 * KEY AGENTS:
 *   - Bhakti sants and gurus: Agenda-setters who interpret scripture and transmit devotional authority through teacher-lineages.
 *   - Hereditary priesthood: Payer/targetâBrahmin ritual specialists whose birth-based monopoly is eroded by devotional legitimacy.
 *   - Bhakti practitioners: Diffuse beneficiary classâdevotees who gain direct spiritual access without hereditary intermediation.
 *   - Persistently marginalized castes: Payer/targetâthose who remain socially subordinated despite the theological opening.
 *   - Reformist secular critics: Excluded voices who reject scriptural authority and demand constitutional equality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading of Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social/interpretive").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '295f1041-188c-42e2-b904-05013aa4444b').
narrative_ontology:cs_kernel_codification('295f1041-188c-42e2-b904-05013aa4444b', fixed_text).
narrative_ontology:cs_authority_grounding('295f1041-188c-42e2-b904-05013aa4444b', lineage).
narrative_ontology:cs_interpretation_layer_present('295f1041-188c-42e2-b904-05013aa4444b').
narrative_ontology:cs_reading_relation('295f1041-188c-42e2-b904-05013aa4444b', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('295f1041-188c-42e2-b904-05013aa4444b', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('295f1041-188c-42e2-b904-05013aa4444b', foundational, devotion_supersedes_birth_in_divine_access).
narrative_ontology:cs_axiom_status(devotion_supersedes_birth_in_divine_access, holdable).
narrative_ontology:cs_axiom_grounding('295f1041-188c-42e2-b904-05013aa4444b', devotion_supersedes_birth_in_divine_access, theological).
narrative_ontology:cs_axiom('295f1041-188c-42e2-b904-05013aa4444b', foundational, personal_deity_relationship_as_normative_ideal).
narrative_ontology:cs_axiom_status(personal_deity_relationship_as_normative_ideal, holdable).
narrative_ontology:cs_axiom_grounding('295f1041-188c-42e2-b904-05013aa4444b', personal_deity_relationship_as_normative_ideal, theological).
narrative_ontology:cs_reference_frame('295f1041-188c-42e2-b904-05013aa4444b', devotional_supremacy_over_birth).
narrative_ontology:cs_drift_state('295f1041-188c-42e2-b904-05013aa4444b', contemporary_religious_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('295f1041-188c-42e2-b904-05013aa4444b', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_practitioners).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, persistently_marginalized_castes).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_as_legitimate_path).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Vedic-Dharmic corpus to privilege devotional sincerity and personal deity-relationship over Brahmin birth-status; establish sampradayas that transmit religious authority through initiation and demonstrated devotion rather than heredity; compete with hereditary priests for congregational followings and temple influence.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_sants_and_gurus, agenda_setter,
    organized, generational, identity_locked, national).

% Hold ritual and interpretive monopoly rooted in varna birth-claims; experience erosion of exclusive authority as devotional movements legitimize non-Brahmin spiritual leadership; must assimilate bhakti motifs or defend orthodox caste prerogatives to retain status, livelihood, and temple roles.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_priesthood, payer,
    institutional, generational, constrained, national).

% Gain direct spiritual access and congregational participation without Brahmin intermediation; experience theological dignity and reduced ritual costs, though lower-caste practitioners continue to face social discrimination and temple-entry barriers that the devotional rhetoric does not fully dismantle.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_practitioners, beneficiary,
    moderate, biographical, constrained, regional).

% Receive promises of egalitarian spiritual inclusion from devotional discourse yet continue to encounter caste-based exclusion in temple administration, ritual roles, marriage markets, and everyday social status; bear the residual cost of a hierarchy that the reading critiques but leaves structurally intact.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, persistently_marginalized_castes, payer,
    powerless, generational, trapped, local).

% Advocate constitutional equality and rational critique of scriptural authority; are structurally excluded from theological debates that take Vedic textual authority as axiomatic, even when those debates concern caste reform.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_secular_critics, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual practice across birth-based caste boundaries by legitimizing direct devotional relationship with the divine as sufficient for religious authority, thereby reducing dependence on hereditary priestly mediation for salvation and worship.
% TRANSFER_FUNCTION: Moves ritual authority, congregational legitimacy, and spiritual status from hereditary Brahmin lineages to devotional leaders and practitioners; shifts religious capital from ritual exclusivity to demonstrated piety.
% ABSENT_VOICES: Reformist secular critics who reject scriptural authority and demand constitutional rather than theological equality; lower-caste activists who view devotional inclusion as insufficient without dismantling social caste; women barred from institutional leadership despite devotional rhetoric of universal access.
% DISAPPEARANCE_RATIONALE: If the bhakti reading vanished, hereditary priesthood would regain exclusive spiritual authority, devotional movements would lose scriptural legitimacy, and millions of non-Brahmins would lose their primary theological basis for direct divine accessâreligious life would reorganize around birth-based monopoly.
% FOUNDING_PROBLEM: Hereditary ritual monopoly restricted divine access, scriptural interpretation, and spiritual authority to Brahmin birth, creating theological exclusion and spiritual dependency for lower castes and women.
% FOUNDING_PROBLEM_CORROBORATION: Lower-caste devotional saints and anti-caste historians attest to historical exclusion; hereditary priests contest that varna order is divinely ordained and not a problem to be solved. Independent textual scholars outside the devotional tradition corroborate the tension between egalitarian hymns and prescriptive caste texts.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.40 to reflect the expected structural delta: the reading genuinely coordinates spiritual access, reducing priestly monopoly, yet does not eliminate caste hierarchy, leaving persistent extraction on marginalized castes. Suppression is 0.35 because the reading spreads primarily through persuasion, hymnody, and institutional adoption, though social sanction against dissenters and apostates remains real. Theater ratio is 0.25: performative devotion exists, but the core coordination functionâmediating divine access across casteâremains operative. Accessibility collapse is 0.40 because hereditary ritual and reformist rationalism remain live alternatives. Resistance is 0.55 from orthodox institutions defending birth-based prerogatives.
 *
 * PERSPECTIVAL GAP:
 *   Hereditary priests experience the reading as a threatening extraction that undermines legitimate order (high d, high effective extraction). Marginalized castes experience an incomplete rope: theological benefit is real but social extraction persists (moderate d). Devotional practitioners experience net benefit or low extraction (low d). The engine computes these divergences from the structural data without requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (bhakti_practitioners) have constrained exit and moderate power, placing them toward the beneficiary end of directionality. Victims include the hereditary_priesthood (constrained exit, institutional power, but structurally targeted by the reading's erosion of their monopoly) and persistently_marginalized_castes (trapped exit, powerless, bearing residual hierarchy). No directionality overrides are required: the beneficiary and victim declarations plus exit options correctly derive the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhereditary exclusivity in divine accessâis partially solved by this reading, which is why it persists across centuries. It has not atrophied into a piton because its coordination function remains live for millions of practitioners. It is not a scaffold because it carries no sunset clause and is not framed as transitional. The rope classification captures the live coordination; the authored moderate extractiveness and residual victim set prevent mislabeling the constraint as either pure extraction or a fully resolved coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    devotional_access_vs_social_mobility,
    'Does the bhakti reading merely provide spiritual access while leaving social caste intact, or does it function as a vector for genuine social emancipation?',
    'Comparative ethnographic and longitudinal study of caste outcomes in devotional communities versus orthodox communities across multiple generations.',
    'If spiritual access does not translate into social equality, the victim set remains larger than the reading''s theology claims, raising effective extraction for marginalized castes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_access_vs_social_mobility, empirical, 'Spiritual access versus social emancipation ambiguity').

omega_variable(
    institutional_replication_of_caste,
    'Do bhakti institutions and sampradayas replicate caste hierarchy internally, for example by privileging Brahmin-born saints or gurus?',
    'Sociological mapping of leadership and authority structures within major bhakti lineages by birth-background and caste status.',
    'If replication is extensive, the coordination benefit is captured by a disguised hereditary structure, pushing the constraint from rope toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_replication_of_caste, empirical, 'Internal caste replication in devotional institutions').

omega_variable(
    kernel_reading_contest,
    'Is this constraint best understood as a distinct reading of a fixed textual kernel, or as an independent theological innovation that has displaced the kernel?',
    'Textual-historical analysis of the relationship between bhakti hymns and Vedic prescriptive caste texts; determination of whether the kernel is genuinely shared or a retroactive construction.',
    'If the kernel is not shared, the reading_relations to sibling constraints dissolve and the constraint becomes an independent coordination mechanism rather than a member of a commitment system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel sharedness and reading independence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(vedi_tr_t100, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(vedi_be_t100, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 100, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_dharmic_corpus__bhakti_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Vedic-Dharmic corpus' conflates three structurally distinct readings: hereditary monopoly (high extraction, birth-based), bhakti devotional (moderate extraction, devotion-based), and reformist egalitarian (low extraction, rights-based). Each reading has distinct epsilon values, beneficiary/victim structures, and authority groundings. They are modeled as a constraint family linked by network edges and cs_structure.reading_relations, not as a single constraint with measurement-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
