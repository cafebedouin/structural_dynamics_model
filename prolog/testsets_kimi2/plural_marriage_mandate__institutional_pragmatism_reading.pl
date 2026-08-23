% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: Plural Marriage Mandate â Institutional Pragmatism Reading (1890 Manifesto)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_pragmatism_reading
 *   of the plural_marriage_mandate kernel. It treats the 1890 Manifesto not
 *   as genuine prophetic revelation (endogenous_reinterpretation_reading) nor
 *   as pure federal coercion voiding legitimacy (exogenous_override_reading),
 *   but as strategic institutional adaptation in which the church leadership
 *   deployed revelatory narrative to legitimate survival-driven capitulation
 *   to superior federal power. The standing arrangement under contest is the
 *   post-1890 regime: public doctrinal suspension, private secret
 *   continuations (1890-1904), and the transfer of survival costs onto
 *   coerced polygamists and deceived monogamists. The beneficiary set is the
 *   church leadership (institutional survival, restored political rights);
 *   the victim set includes coerced polygamists (forced abandonment of
 *   families under doctrinal duress) and deceived monogamists (compliance
 *   secured under false pretenses of permanent revelatory change). The
 *   constraint type is tangled_rope: it carries a genuine coordination
 *   function (church-state dÃ©tente, institutional survival) entangled with
 *   asymmetric extraction (doctrinal deception, cost transfer to members).
 *
 * KEY AGENTS:
 *   - church_leadership: Primary agenda_setter and beneficiary (institutional/arbitrage) â secures survival and political rights via revelatory performance.
 *   - coerced_polygamists: Primary target (moderate/identity_locked) â bears the costs of family dissolution and doctrinal betrayal.
 *   - deceived_monogamists: Secondary target (moderate/identity_locked) â bears the costs of institutional deception and loss of transparent governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.74).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.8).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "Plural Marriage Mandate â Institutional Pragmatism Reading (1890 Manifesto)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '23d37517-01d5-4ae9-9255-e19e5def3a50').
narrative_ontology:cs_kernel_codification('23d37517-01d5-4ae9-9255-e19e5def3a50', fixed_text).
narrative_ontology:cs_authority_grounding('23d37517-01d5-4ae9-9255-e19e5def3a50', lineage).
narrative_ontology:cs_interpretation_layer_present('23d37517-01d5-4ae9-9255-e19e5def3a50').
narrative_ontology:cs_reading_relation('23d37517-01d5-4ae9-9255-e19e5def3a50', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('23d37517-01d5-4ae9-9255-e19e5def3a50', plural_marriage_mandate__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('23d37517-01d5-4ae9-9255-e19e5def3a50', foundational, institutional_survival_supersedes_doctrinal_transparency).
narrative_ontology:cs_axiom_status(institutional_survival_supersedes_doctrinal_transparency, holdable).
narrative_ontology:cs_axiom_grounding('23d37517-01d5-4ae9-9255-e19e5def3a50', institutional_survival_supersedes_doctrinal_transparency, instrumental).
narrative_ontology:cs_axiom('23d37517-01d5-4ae9-9255-e19e5def3a50', foundational, revelation_narrative_as_legitimation_tool).
narrative_ontology:cs_axiom_status(revelation_narrative_as_legitimation_tool, holdable).
narrative_ontology:cs_axiom_grounding('23d37517-01d5-4ae9-9255-e19e5def3a50', revelation_narrative_as_legitimation_tool, empirically_contingent).
narrative_ontology:cs_reference_frame('23d37517-01d5-4ae9-9255-e19e5def3a50', prophetic_revelation_authority).
narrative_ontology:cs_drift_state('23d37517-01d5-4ae9-9255-e19e5def3a50', post_1904_exposure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23d37517-01d5-4ae9-9255-e19e5def3a50', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto claiming prophetic revelation to suspend plural marriage, while privately understanding this as necessary capitulation to federal coercive pressure. Retained doctrinal authority, secured church property and political rights, and managed secret plural marriage continuations from 1890 to 1904.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Believed plural marriage was a divine commandment required for exaltation. Were pressured by church leadership to abandon existing plural families after 1890 under threat of excommunication and federal prosecution. Experienced the manifesto as a betrayal of doctrinal promises; secret continuations by leadership proved the public doctrine was performative.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    moderate, biographical, identity_locked, national).

% Accepted the 1890 Manifesto as a genuine, permanent revelation ending plural marriage. Were not informed of secret continuations (1890-1904) or the doctrinal preservation of plural marriage theology. Their trust in prophetic transparency was exploited to secure compliance and political rehabilitation of the church.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Mormon community's survival under existential federal pressure by producing a single authoritative directive that halts open plural marriage, thereby securing property, voting rights, and corporate legal standing for the institutional church.
% TRANSFER_FUNCTION: Moves institutional survival, political rights, and property security from the federal sphere to the church leadership; moves the costs of family dissolution, doctrinal confusion, and compliance pressure from leadership to coerced polygamists and deceived monogamists.
% ABSENT_VOICES: Fundamentalist splinter groups who rejected the manifesto and continued plural marriage were excommunicated and excluded from institutional voice; theological dissenters who questioned the revelatory framing were silenced by apostasy charges.
% DISAPPEARANCE_RATIONALE: Without the manifesto and its legitimation structure, the church faced probable disincorporation and property seizure; polygamists would have continued openly; monogamists would not have accepted the doctrinal shift; the Utah-American political settlement would have followed a different path.
% FOUNDING_PROBLEM: Existential destruction of the church via federal anti-polygamy legislation (Edmunds-Tucker Act 1887), including disincorporation, property confiscation, and disenfranchisement of members.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records, territorial governors' correspondence, and external legal historians corroborate the existential threat and its resolution through Utah statehood (1896). Church historians inside the beneficiary set acknowledge political necessity but embed it within revelatory narrative; no external non-beneficiary corroborates the continuing revelatory framing.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the arrangement transfers the full costs of federal capitulation onto specific member populations while leadership captures institutional and political benefits. Suppression (0.80) is high because the constraint's persistence requires active concealment of secret continuations, suppression of fundamentalist dissent, and maintenance of the revelatory fiction. Theater ratio (0.55) is moderate-high: the manifesto's revelatory framing is largely performative, masking pragmatic political calculation, though some genuine coordination (church survival) is present. Accessibility collapse (0.72) is high because prophetic authority and identity fusion within the Mormon community made questioning the manifesto spiritually and socially prohibitive. Resistance (0.48) is moderate: fundamentalist splinter groups and some dissenters resisted, but the majority complied under identity pressure. The measurement series run on a shared time grid (0, 5, 10, 14, 20, 25, 30) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat, the manifesto appears as necessary coordination to prevent institutional annihilation; the costs to members are framed as temporary sacrifices for collective survival. From the coerced polygamist seat, the same structure appears as a betrayal of divine promises and forced family dissolution under duress. From the deceived monogamist seat, it appears as a violation of fiduciary transparency in religious governance. The engine computes divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership sits near the full-beneficiary end (d approaching 0.0): they authored the constraint, control its narrative, and directly receive institutional survival and political restoration. Coerced polygamists sit near the full-target end (d approaching 1.0): they were the population directly compelled to abandon families and accept doctrinal reversal under threat of excommunication and federal prosecution. Deceived monogamists also sit near the target end (d approximately 0.85): their compliance was secured through false representations about the permanence and genuineness of the revelatory change. The identity_locked exit option amplifies effective extraction for both victim groups because prophetic authority fuses compliance with salvation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâexistential federal destruction via the Edmunds-Tucker Actâwas resolved by Utah statehood (1896) and the manifesto's political effect. Yet the constraint persisted: secret continuations ran until 1904, the revelatory narrative was never retracted or reframed as pragmatic adaptation, and the doctrinal apparatus continued to treat the manifesto as genuine revelation. This is mandatrophy: the arrangement outlived its survival function but continued because acknowledging its pragmatic origins would collapse the prophetic authority structure that justified it. The theater_ratio measurements show sustained performative activity after the threat window closed, corroborating the mandatrophy diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secret_continuation_scope,
    'Did post-1890 secret plural marriages represent deliberate institutional deception, or a theologically permitted transitional buffer?',
    'Archival access to internal First Presidency and Quorum of the Twelve communications, 1890-1904, documenting authorization versus toleration.',
    'If deliberate deception, the extraction from deceived monogamists is maximal and the constraint leans snare-like; if transitional buffer, the constraint retains more genuine coordination character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secret_continuation_scope, empirical, 'Whether secret continuations were authorized deception or transitional tolerance.').

omega_variable(
    polygamist_compliance_nature,
    'Did coerced polygamists comply primarily from identity-fused religious obedience, or from dual federal-institutional survival pressure?',
    'Diary, oral history, and excommunication record analysis measuring stated motivations and experienced coercion.',
    'Identity-locked compliance raises effective extraction beyond structural measures because the target internalizes the constraint; pressure-driven compliance confirms structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polygamist_compliance_nature, empirical, 'Internalized versus structural drivers of polygamist compliance.').

omega_variable(
    doctrine_practice_separability,
    'Can the church''s institutional survival (coordination function) be structurally separated from the doctrinal deception (extractive component), or are they inseparable in this case?',
    'Comparative analysis of other religious communities facing existential state pressure to determine whether survival without doctrinal performance is achievable.',
    'If separable, the constraint is a tangled rope with separable coordination and extraction; if inseparable, the entire structure functions as a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_separability, conceptual, 'Separability of coordination and extraction in institutional survival.').

omega_variable(
    mandatrophy_timing,
    'When did the existential federal threat endâ1890, 1896 (statehood), or 1904âsuch that the constraint''s founding problem was solved?',
    'Historical analysis of federal enforcement intensity, legal standing, and prosecutorial discretion toward polygamists.',
    'Determines the date after which the constraint persisted beyond its founding problem, shifting from scaffold-like emergency survival to inertial or extractive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_timing, empirical, 'Timing of founding problem resolution for mandatrophy analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t0, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t5, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t10, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t14, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 14, 0.6).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t20, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t25, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 25, 0.56).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_tr_t30, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t0, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t5, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t10, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t14, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 14, 0.76).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t20, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t25, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_be_t30, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 30, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t0, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t5, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t10, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t14, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 14, 0.86).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t20, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t25, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(plural_marriage_mandate_inst_prag_su_t30, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three structurally distinct constraint stories. Each reading carries a different epsilon, beneficiary/victim structure, and classification: endogenous_reinterpretation (legitimate prophecy, low extraction for believers), exogenous_override (pure coercion, victim set may shift to entire church), and institutional_pragmatism (doctrinal legitimation of capitulation, tangled_rope). They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
