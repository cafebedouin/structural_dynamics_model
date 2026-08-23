% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__theistic_evolutionary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__theistic_evolutionary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: genesis_creation_narrative__theistic_evolutionary
 *   human_readable: Theistic Evolutionary Reading of Genesis 1-2
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   The theistic evolutionary reading of Genesis 1-2 emerged in the late 19th
 *   century as a response to Darwinian biology and geological deep time. It
 *   treats the creation narrative as a theological framework — using literary
 *   devices (framework hypothesis, day-age, analogical days) to communicate
 *   doctrinal truths (God as creator, humanity as image-bearers, creation as
 *   good) without making historical-scientific claims about chronology or
 *   mechanism. This reading coordinates religious identity with scientific
 *   literacy, functioning as a rope: it solves a genuine coordination problem
 *   (faith-science tension) with minimal coercion, and its adherents are net
 *   beneficiaries. The constraint's extractiveness is low (cognitive effort
 *   of hermeneutical integration), suppression is negligible (it does not
 *   police alternative readings), and theater ratio is low but rising
 *   slightly as institutional infrastructures (BioLogos, denominational
 *   statements) formalize what was once informal consensus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__theistic_evolutionary, 0.12).
domain_priors:suppression_score(genesis_creation_narrative__theistic_evolutionary, 0.08).
domain_priors:theater_ratio(genesis_creation_narrative__theistic_evolutionary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, extractiveness, 0.12).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(genesis_creation_narrative__theistic_evolutionary, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__theistic_evolutionary, rope).
narrative_ontology:human_readable(genesis_creation_narrative__theistic_evolutionary, "Theistic Evolutionary Reading of Genesis 1-2").
narrative_ontology:topic_domain(genesis_creation_narrative__theistic_evolutionary, "religious_studies/biblical_hermeneutics/science_religion_interface").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__theistic_evolutionary, '41065618-fb72-404a-98a0-5d251070d3be').
narrative_ontology:cs_kernel_codification('41065618-fb72-404a-98a0-5d251070d3be', fixed_text).
narrative_ontology:cs_authority_grounding('41065618-fb72-404a-98a0-5d251070d3be', lineage).
narrative_ontology:cs_interpretation_layer_present('41065618-fb72-404a-98a0-5d251070d3be').
narrative_ontology:cs_reading_relation('41065618-fb72-404a-98a0-5d251070d3be', genesis_creation_narrative__literal_young_earth, coexists_with).
narrative_ontology:cs_reading_relation('41065618-fb72-404a-98a0-5d251070d3be', genesis_creation_narrative__allegorical_ancient_near_east, coexists_with).
narrative_ontology:cs_axiom('41065618-fb72-404a-98a0-5d251070d3be', foundational, scripture_and_nature_harmonize).
narrative_ontology:cs_axiom_status(scripture_and_nature_harmonize, holdable).
narrative_ontology:cs_axiom_grounding('41065618-fb72-404a-98a0-5d251070d3be', scripture_and_nature_harmonize, empirically_contingent).
narrative_ontology:cs_axiom('41065618-fb72-404a-98a0-5d251070d3be', foundational, human_dominion_entails_stewardship).
narrative_ontology:cs_axiom_status(human_dominion_entails_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('41065618-fb72-404a-98a0-5d251070d3be', human_dominion_entails_stewardship, deontological).
narrative_ontology:cs_reference_frame('41065618-fb72-404a-98a0-5d251070d3be', concordist_hermeneutic).
narrative_ontology:cs_drift_state('41065618-fb72-404a-98a0-5d251070d3be', contemporary_post_genomic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('41065618-fb72-404a-98a0-5d251070d3be', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, theistic_evolutionists).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, science_accepting_christians).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, theological_institutions_teaching_compatibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__theistic_evolutionary, religious_educators).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, evolutionary_creationism).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, day_age_interpretation).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, framework_interpretation).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__theistic_evolutionary, stewardship_dominion_ethic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Christians who affirm evolutionary science and read Genesis 1-2 as theological literature compatible with deep time. They use this reading to maintain coherent identity across scientific and religious communities. Exit means either adopting literalism (cognitive cost) or secularism (identity cost), but both options are structurally available.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, theistic_evolutionists, beneficiary,
    moderate, biographical, mobile, global).

% Believers in scientific professions or education who need a hermeneutic that does not require rejecting their work. This reading lowers the cost of staying in faith communities. Their exit options are constrained by social and familial ties to church communities that may not accept this reading.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, science_accepting_christians, beneficiary,
    moderate, biographical, constrained, global).

% Seminaries, denominations, and parachurch organizations (e.g., BioLogos, Faraday Institute) that develop, credential, and disseminate this reading. They set curricula, publish resources, and define orthodoxy boundaries. They can shift emphasis or adopt alternative readings with moderate institutional cost.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, theological_institutions_teaching_compatibility, agenda_setter,
    institutional, generational, arbitrage, global).

% Scientists who study evolution and engage public understanding. They benefit indirectly when religious opposition to evolution decreases, but they do not control the reading. Their professional standing is independent of theological debates.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, evolutionary_biologists, observer,
    analytical, civilizational, analytical, universal).

% Academic specialists in Ancient Near Eastern literature, Genesis exegesis, and reception history. They provide the philological and historical evidence for literary-framework and day-age readings. Their work constrains but does not determine theological adoption.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, biblical_scholars, observer,
    analytical, generational, analytical, global).

% Pastors, teachers, and campus ministers who transmit this reading to congregations and students. They benefit from having a coherent curriculum that avoids faith-science conflict. They bear reputational cost in conservative contexts but have mobility across denominational lines.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__theistic_evolutionary, religious_educators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__theistic_evolutionary, religious_educators, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Christians to affirm both theological creation doctrine and evolutionary science without cognitive dissonance or communal rupture; provides hermeneutical framework where Genesis 1-2 functions as theological anthropology rather than scientific chronicle.
% TRANSFER_FUNCTION: Moves interpretive authority from literal-historical reading to literary-theological reading; shifts epistemic load from scripture-as-science-text to scripture-as-theological-witness; transfers legitimacy to scientific consensus on cosmology and biology.
% ABSENT_VOICES: Literal young-earth creationists and their institutions, who view this reading as compromise or unfaithfulness; they are structurally excluded from the conversations where this reading is formulated (mainline seminaries, scientific organizations, ecumenical dialogues).
% DISAPPEARANCE_RATIONALE: This reading currently stabilizes the faith-science interface for millions of Christians; its removal would create a vacuum forcing choice between scientific literacy and theological fidelity, restructuring church education, seminary curricula, and public witness.
% FOUNDING_PROBLEM: Post-Darwin crisis: how to maintain Christian theological integrity while accepting the emerging scientific consensus on common descent and deep time, without retreating into anti-intellectualism or abandoning scriptural authority.
% FOUNDING_PROBLEM_CORROBORATION: Historical theology (e.g., B.B. Warfield, Asa Gray, Pierre Teilhard de Chardin) and contemporary science-religion scholarship (e.g., BioLogos, Faraday Institute) attest the problem persists; even YEC organizations implicitly acknowledge it by devoting massive resources to alternative frameworks.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__theistic_evolutionary, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__theistic_evolutionary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__theistic_evolutionary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__theistic_evolutionary, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__theistic_evolutionary, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__theistic_evolutionary_tests).
:- end_tests(genesis_creation_narrative__theistic_evolutionary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12) reflects the modest cognitive and social cost of maintaining a non-literal hermeneutic in communities where literalism is normative. Suppression (0.08) is near-zero because this reading does not enforce adherence — it persuades through exegetical argument and scientific evidence. Theater ratio (0.15) captures the growing institutional performance (conferences, statements, curricula) that exceeds the functional core of the reading. Accessibility collapse (0.25) is low because literalist and allegorical alternatives remain fully accessible and actively advocated. Resistance (0.35) comes from fundamentalist and conservative evangelical networks that treat this reading as heterodoxy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (theological institutions) experiences this as a coordination achievement — they built a stable hermeneutic that preserves orthodoxy while engaging science. The beneficiary seats (theistic evolutionists, science-accepting Christians) experience it as liberation from false choice. The observer seats (biologists, scholars) see it as a successful cultural adaptation. No seat experiences significant extraction; the divergence is in perceived necessity, not cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (theistic_evolutionists, science_accepting_christians, religious_educators) receive the coordination good: a hermeneutic that lets them inhabit both scientific and religious worlds. The agenda_setter (theological_institutions) invests in producing and maintaining this good but also gains institutional legitimacy and retention. No stakeholder bears net extraction costs — the literal_young_earth_adherents are not parties to this constraint; they are parties to a sibling constraint. Directionality for all seats is near-symmetric (d ≈ 0.4–0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling Christian theology with evolutionary science) remains live — new genomic evidence, cultural polarization, and generational turnover renew the need. The reading has not atrophied into piton; its theater ratio rise reflects institutionalization of a still-functional coordination, not substitution of performance for function. No mandatrophy resolution needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint a distinct reading of the genesis_creation_narrative kernel, or a variant within a single reading?',
    'Compare ε, suppression, and beneficiary/victim structures across the three declared readings. If each has stable, distinct metric profiles and non-overlapping stakeholder sets, they are separate constraints.',
    'If confirmed as separate constraints, the kernel decomposition is validated and network edges between them can be traced. If they collapse to one constraint, the kernel model over-partitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the three declared readings instantiate three ε-invariant constraints or one constraint with measurement variance.').

omega_variable(
    structural_delta_coercion_boundary,
    'Does the theistic_evolutionary reading exert any structural coercion on literal_young_earth adherents (e.g., through institutional credentialing, employment, or social pressure), or is the relationship purely competitive in the marketplace of interpretations?',
    'Survey employment policies at Christian colleges, denomination ordination requirements, and publishing gatekeeping. If theistic_evolutionary institutions structurally exclude literal_young_earth adherents from professional roles, coercion exists.',
    'If coercion exists, suppression and extraction metrics for this reading are understated; the reading would shift toward tangled_rope. If purely competitive, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_coercion_boundary, empirical, 'Whether the coordination function carries hidden extraction toward the sibling reading''s adherents.').

omega_variable(
    day_age_vs_framework_hermeneutic,
    'Are day-age and framework-hypothesis sub-readings structurally distinct constraints (different ε, different stakeholder coalitions) or variations within this reading?',
    'Map stakeholder affiliations: day-age advocates (e.g., Reasons to Believe) vs. framework-hypothesis advocates (e.g., BioLogos). If they form distinct institutional networks with different suppression profiles, decompose.',
    'If distinct, this story over-aggregates; two JSON files needed with network links. If unified, current aggregation is valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(day_age_vs_framework_hermeneutic, conceptual, 'Whether internal hermeneutical differences map to structural constraint differences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__theistic_evolutionary, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_theistic_evolutionary_tr_t0, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_tr_t0, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_tr_t20, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_tr_t20, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_tr_t40, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_tr_t40, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_tr_t60, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_tr_t60, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_tr_t80, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 80, 0.14).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_tr_t80, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_tr_t100, genesis_creation_narrative__theistic_evolutionary, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(genesis_theistic_evolutionary_be_t0, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_be_t0, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_be_t20, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 20, 0.11).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_be_t20, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_be_t40, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_be_t40, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_be_t60, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 60, 0.12).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_be_t60, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_be_t80, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 80, 0.12).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_be_t80, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_be_t100, genesis_creation_narrative__theistic_evolutionary, base_extractiveness, 100, 0.12).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(genesis_theistic_evolutionary_su_t0, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_su_t0, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_su_t20, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 20, 0.06).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_su_t20, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_su_t40, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 40, 0.07).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_su_t40, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_su_t60, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 60, 0.08).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_su_t60, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_su_t80, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 80, 0.08).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_su_t80, observed).
narrative_ontology:measurement(genesis_theistic_evolutionary_su_t100, genesis_creation_narrative__theistic_evolutionary, suppression_requirement, 100, 0.08).
narrative_ontology:measurement_basis(genesis_theistic_evolutionary_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__theistic_evolutionary, identity_coordination).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__theistic_evolutionary, 0.08).
narrative_ontology:affects_constraint(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__allegorical_ancient_near_east).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the genesis_creation_narrative kernel. The literal_young_earth reading (high extraction, high suppression, claimed_type: snare) and allegorical_ancient_near_east reading (low extraction, different coordination function, claimed_type: rope) are sibling constraints. All three share the fixed textual kernel but instantiate different constraint structures with different ε values. The theistic_evolutionary reading's ε (0.12) differs from literal_young_earth (estimated >0.7) and allegorical_ancient_near_east (estimated ~0.05), confirming ε-invariance requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
