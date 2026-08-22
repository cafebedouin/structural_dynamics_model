% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura Authority Constraint
 *   domain: theology/religious_studies
 *
 * SUMMARY:
 *   The sola scriptura reading of the biblical authority kernel holds that
 *   the canonical scriptures are the sole sufficient and self-interpreting
 *   norm for Christian doctrine and practice. It emerged from the Protestant
 *   Reformation as an alternative to magisterial and tradition-based
 *   authority structures, claiming that scripture's perspicuity makes it
 *   accessible to believer and clergy alike without magisterial mediation.
 *   This constraint story models the standing arrangement of this authority
 *   claim as a coordination mechanism for Protestant communities, noting its
 *   structural reduction of clerical extraction and its structural cost to
 *   trans-denominational doctrinal coherence. It is authored as one reading
 *   of a contested kernel; the tradition-scripture and conciliar readings are
 *   structurally linked as siblings but are not evaluated within this
 *   constraint.
 *
 * KEY AGENTS:
 *   - lay_believers (beneficiary - gain interpretive autonomy)
 *   - protestant_congregations (beneficiary - local self-governance)
 *   - protestant_denominations (agenda_setter - enforce scriptural boundaries)
 *   - ecumenical_church_bodies (payer - bear fragmentation costs)
 *   - catholic_orthodox_magisterium (excluded - would argue for tradition)
 *   - biblical_academy (observer - studies text historically)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.38).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.42).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura Authority Constraint").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '99363525-362f-4794-9cd3-7a83a21d7885').
narrative_ontology:cs_kernel_codification('99363525-362f-4794-9cd3-7a83a21d7885', fixed_text).
narrative_ontology:cs_authority_grounding('99363525-362f-4794-9cd3-7a83a21d7885', self_enforcing).
narrative_ontology:cs_reading_relation('99363525-362f-4794-9cd3-7a83a21d7885', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('99363525-362f-4794-9cd3-7a83a21d7885', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('99363525-362f-4794-9cd3-7a83a21d7885', foundational, scripture_alone_sufficient).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('99363525-362f-4794-9cd3-7a83a21d7885', scripture_alone_sufficient, theological).
narrative_ontology:cs_axiom('99363525-362f-4794-9cd3-7a83a21d7885', foundational, scripture_perspicuous_to_believer).
narrative_ontology:cs_axiom_status(scripture_perspicuous_to_believer, holdable).
narrative_ontology:cs_axiom_grounding('99363525-362f-4794-9cd3-7a83a21d7885', scripture_perspicuous_to_believer, theological).
narrative_ontology:cs_reference_frame('99363525-362f-4794-9cd3-7a83a21d7885', scriptural_self_sufficiency).
narrative_ontology:cs_drift_state('99363525-362f-4794-9cd3-7a83a21d7885', contemporary_denominational_plurality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99363525-362f-4794-9cd3-7a83a21d7885', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, protestant_congregations).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, ecumenical_church_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise interpretive autonomy over scripture in personal devotion and congregational life, accessing divine authority without magisterial mediation. Exit is constrained by identity and community tiesâconverting to tradition-based churches means abandoning spiritual heritage and social networks.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, constrained, global).

% Operate with local autonomy over doctrine and practice, free from hierarchical magisterial control. They benefit from the authority structure by gaining self-governance, though they remain bound to scripture as the formal criterion for discipline and teaching.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, protestant_congregations, beneficiary,
    organized, generational, constrained, global).

% Administer ordination, disciplinary boundaries, and confessional standards using scripture as the formal criterion. They enforce the constraint by excluding teachings that require tradition or magisterial authority for validation, yet cannot extract magisterial rents due to the sufficiency premise.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, protestant_denominations, agenda_setter,
    organized, generational, constrained, national).

% Work for visible unity among Christian communities. Bear the structural cost of sola scriptura's interpretive pluralismâevery denomination reads scripture differently, making shared doctrinal statements nearly impossible. Cannot exit because their founding mission assumes unity across precisely these divisions.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_church_bodies, payer,
    organized, civilizational, trapped, global).

% Represent the historic alternative where scripture and tradition are inseparable. Would object that sola scriptura is a sixteenth-century innovation lacking patristic support, but are structurally excluded from Protestant authority conversations.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, catholic_orthodox_magisterium, excluded,
    institutional, civilizational, analytical, global).

% Historical-critical scholars who study textual origins and transmission. Their findings sometimes destabilize the self-interpreting claim, yet they operate outside the constraint's enforcementâneither benefited nor extracted from by the authority structure itself.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, biblical_academy, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates doctrine and practice among dispersed Protestant communities without a centralized magisterium by positing a single, accessible authority source that every believer can read and every congregation can adjudicate.
% TRANSFER_FUNCTION: Moves authority from centralized magisterial institutions to individual believers and local congregations, while transferring the cost of interpretive fragmentation to trans-denominational unity movements and the broader Christian church.
% ABSENT_VOICES: Catholic and Orthodox magisterial authorities, as well as tradition-affirming theologians within Protestantism, are structurally excluded from the authority conversation; they would argue for the necessity of interpretive tradition but are ruled out by the sufficiency premise.
% DISAPPEARANCE_RATIONALE: If the sufficiency and self-interpreting claims disappeared, Protestant communities would lose their constitutive authority principle. Congregations would either fragment into pure autonomy, reconstitute around confessional traditions with magisterial weight, or migrate toward Catholic and Orthodox structures. The rearrangement would be profound because the constraint organizes the epistemic basis of an entire branch of Christianity.
% FOUNDING_PROBLEM: The Western church in the sixteenth century faced a crisis of magisterial corruption, perceived doctrinal error, and the concentration of interpretive power in a clerical hierarchy that many reformers judged unaccountable and extra-biblical.
% FOUNDING_PROBLEM_CORROBORATION: Protestant historians and theologians attest the problem was real. Catholic and Orthodox historians attest the problem was partial and did not warrant breaking magisterial continuity; they corroborate the abuses but contest the remedy. Independent secular historians corroborate institutional corruption while disputing whether sola scriptura was the only or best solution.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 (moderate-low) because the constraint genuinely displaces magisterial rent-seeking but imposes real costs on ecclesial unity. Suppression at 0.42 reflects moderate institutional enforcementâexclusion from ordination, denominational boundary maintenance, confessional exclusionârather than state violence. Theater ratio at 0.25 indicates that most activity remains functional (preaching, exegesis, confessional formulation), though some performative boundary-policing exists. Accessibility collapse at 0.72 captures how, once the sola scriptura frame is accepted, tradition-based alternatives appear as human corruptions rather than legitimate authorities. Resistance at 0.48 registers ongoing Catholic, Orthodox, and liberal Protestant resistance. The temporal series show slow accumulation of extraction and theater as the movement institutionalized over centuries.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (lay believers experiencing autonomy) and the payer seat (ecumenical bodies experiencing fragmentation) compute different types from the same structural data: from the lay perspective the constraint is liberating coordination, while from the ecumenical perspective it is a persistent obstacle to unity. The agenda-setter seat (denominations) experiences a mixed positionâthey enforce the constraint but are also constrained by it (they cannot invent magisterial authority without betraying the founding premise).
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and Protestant congregations are structural beneficiaries: the constraint subsidizes their direct access to divine authority and self-governance (low d, low effective extraction). Protestant denominations sit near symmetric: they enforce the constraint and gain institutional existence from it, but cannot extract magisterial rents (d ~0.4). Ecumenical church bodies are structural targets: the constraint directly undermines their founding purpose by licensing unlimited interpretive divergence (high d, high effective extraction). Catholic and Orthodox magisterium is excludedâthey fall outside the constraint's scope and their exclusion is constitutive of the constraint's identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) because the coordination function is genuine and historically demonstrated: it solved the collective-action problem of how Protestant communities agree on anything without a pope. It also prevents mislabeling it as pure coordination (rope) because the same structure that coordinates Protestant identity simultaneously imposes asymmetric costs on the broader Christian church's unity. The mandatrophy question is whether the founding problem (magisterial corruption) is still live; the contested status prevents automatic piton classification despite the arrangement's age.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the sola_scriptura reading of the biblical_authority kernel. How would the beneficiary-victim structure shift if the tradition_scripture or conciliar reading were adopted instead?',
    'Comparative structural analysis of the three kernel readings, measuring the concentration of interpretive authority and the distribution of doctrinal access across the same stakeholder seats.',
    'If sibling readings were adopted, magisterial institutions would move from excluded to agenda_setter or beneficiary, lay believers would lose autonomy (higher d), and ecumenical bodies might move from payer to beneficiary if tradition reduced fragmentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committer-frame omega identifying this constraint as one reading of a contested kernel with structurally distinct siblings.').

omega_variable(
    fragmentation_as_falsification,
    'Does the empirical fact of persistent denominational fragmentation constitute evidence against the self-interpreting sufficiency of scripture?',
    'Comparative study of interpretive divergence rates under sola scriptura versus magisterial or tradition-based frameworks, controlling for political and cultural variables.',
    'If fragmentation falsifies the self-interpreting claim, the constraint''s authority grounding shifts toward practice or extraction and its axioms may be reclassified as overridden; if not, the theological grounding remains holdable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_as_falsification, empirical, 'Whether empirical divergence undermines the self-interpreting axiom.').

omega_variable(
    clerical_extraction_shift,
    'Is the low clerical extraction a genuine structural feature of this reading, or does it conceal extraction shifted to professional biblical scholars and denominational bureaucracies?',
    'Economic and sociological analysis of Protestant clergy income, denominational administrative overhead, and para-church institutional power relative to lay members, compared to Catholic and Orthodox counterparts.',
    'If extraction is shifted rather than eliminated, the constraint may reclassify toward snare or higher-extraction tangled rope from the beneficiary side, invalidating the low-extraction narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerical_extraction_shift, empirical, 'Whether low clerical extraction is real or displaced into other institutional layers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.23).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.25).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.2).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.26).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.31).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement(bibl_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.4).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__sola_scriptura_reading, suppression_requirement, 400, 0.41).
narrative_ontology:measurement(bibl_su_t500, biblical_authority__sola_scriptura_reading, suppression_requirement, 500, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the biblical_authority kernel family. The three readings (sola_scriptura, tradition_scripture, conciliar) are not different measurements of the same constraint but different commitments with different epsilon values and different stakeholder structures. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
