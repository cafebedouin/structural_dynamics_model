% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Text
 *   domain: constitutional_theory_political_philosophy_comparative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the legislative sovereignty reading of
 *   the constitutional_text kernel. Under this reading, constitutional text
 *   establishes the legislature as the supreme authority over constitutional
 *   meaning, reducing judicial review to an advisory function and permitting
 *   legislative override or notwithstanding clauses. The constraint
 *   coordinates democratic governance by aligning constitutional
 *   interpretation with electoral majorities, but asymmetrically extracts
 *   protective capacity from courts and minority rights-bearers. It is
 *   structurally distinct from the judicial_supremacy_reading, which assigns
 *   final authority to courts, and the popular_sovereignty_reading, which
 *   reserves ultimate authority to the constituent power of the people. The
 *   authored claim is tangled_rope: a genuine coordination function for
 *   democratic self-government is coupled with active, asymmetric extraction
 *   from disempowered judicial and minority seats.
 *
 * KEY AGENTS:
 *   - parliamentary_majority: Primary agenda-setter and beneficiary (institutional/arbitrage) â commands legislative override and collects majoritarian policy latitude
 *   - judiciary: Primary target (institutional/constrained) â interpretive authority is extracted through override mechanisms
 *   - rights_bearing_minorities: Secondary target (powerless/constrained) â constitutional rights protection is rendered contingent on legislative grace
 *   - direct_democracy_advocates: Excluded voice (moderate/constrained) â argue for direct popular constitutionalism but are shut out of representative-supremacy frameworks
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) â tracks comparative allocation of constitutional authority without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.72).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.78).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory_political_philosophy_comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'ae0a17d1-ebb2-43c4-9a35-8856e2542b00').
narrative_ontology:cs_kernel_codification('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', formalized).
narrative_ontology:cs_authority_grounding('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', lineage).
narrative_ontology:cs_reading_relation('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', constitutional_text__popular_sovereignty_reading, influences).
narrative_ontology:cs_axiom('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', foundational, legislative_finality_doctrine).
narrative_ontology:cs_axiom_status(legislative_finality_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', legislative_finality_doctrine, conventional).
narrative_ontology:cs_axiom('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', foundational, majoritarian_preference_priority).
narrative_ontology:cs_axiom_status(majoritarian_preference_priority, holdable).
narrative_ontology:cs_axiom_grounding('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', majoritarian_preference_priority, deontological).
narrative_ontology:cs_reference_frame('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', contemporary_rights_charter_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ae0a17d1-ebb2-43c4-9a35-8856e2542b00', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, rights_bearing_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds elected office and commands the legislative agenda. Can pass ordinary legislation or invoke override and notwithstanding clauses to set aside adverse judicial interpretations of constitutional rights and division of powers. Retains the constitutional text as a source of authority while reserving the final word on its meaning. Exit from this position is primarily electoral defeat, but within a parliamentary term the constraint is the majority's instrument of governance.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, beneficiary).

% Hears constitutional challenges and interprets the constitutional text, but knows its rulings on rights or federalism can be set aside by a simple legislative majority through override mechanisms. Must frame advisory opinions knowing they are not final. Bears the institutional cost of reduced interpretive authority relative to systems of judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Rely on constitutional text and judicial process to protect language, religion, association, or dissenting practice against majoritarian legislation. Face the risk that parliament can override the judicial protections they obtain, converting constitutional rights into contingent privileges subject to legislative grace. Their exit options are limited to political mobilization or leaving the jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, rights_bearing_minorities, payer,
    powerless, generational, constrained, national).

% Argue that ultimate constitutional authority should reside in the people directly through referenda, conventions, or constituent assemblies rather than in elected parliaments. Are structurally excluded from legislative sovereignty frameworks because the reading treats parliament as the complete and exclusive expression of popular will, leaving no institutional room for direct constituent power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, direct_democracy_advocates, excluded,
    moderate, civilizational, constrained, national).

% Observe and classify how different jurisdictions allocate final constitutional authority among legislatures, courts, and popular assemblies. Do not bear the constraint's costs or collect its benefits; their position is analytical and comparative.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between elected legislatures and constitutional text by ensuring that democratically accountable representatives, rather than unelected judges or diffuse popular assemblies, hold final interpretive authority, thereby aligning constitutional change and rights limitation with electoral majorities.
% TRANSFER_FUNCTION: Moves final constitutional interpretive authority and the capacity to override rights-protective judgments from courts and minority rights-bearers to the parliamentary majority, allowing the majority to enact its policy preferences even when they conflict with judicial readings of the constitution.
% ABSENT_VOICES: Advocates of judicial supremacy, who view courts as the ultimate guardians of constitutional limits against legislative overreach, and popular sovereignty theorists, who argue that constituent power remains with the people and cannot be fully delegated to representative institutions, are marginalized in legislative sovereignty frameworks; their objections are treated as institutionally illegitimate within this reading.
% DISAPPEARANCE_RATIONALE: If legislative sovereignty and its override mechanisms vanished overnight, courts would gain conclusive interpretive authority, minority rights claimants would shift to litigation-centered strategies, and legislative majorities would face new judicial vetoes on their programs; the institutional architecture would reorganize around judicial supremacy or popular constitutionalism.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: unelected judges with security of tenure setting aside laws enacted by democratically accountable legislatures, thereby entrenching elite or historical preferences against contemporary majoritarian will and depriving current electorates of final self-governing authority.
% FOUNDING_PROBLEM_CORROBORATION: Political constitutionalists such as Jeremy Waldron and Richard Bellamy attest the problem from an analytical seat outside the immediate parliamentary beneficiary class; judicial supremacy advocates such as Ronald Dworkin and rights-based constitutionalists contest the problem's framing, corroborating the contested status from an opposing normative position.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the override mechanism allows parliamentary majorities to dispense with constitutional rights protections and judicial interpretations that would otherwise constrain them, transferring authority to the electoral winner. Suppression (0.78) is higher still because the constraint depends on actively maintaining the legislative override against judicial resistance and rights-based mobilization; the legislature must repeatedly assert its supremacy to prevent drift toward judicial supremacy. Theater_ratio (0.45) is moderate: legislative debate and constitutional forms are performed, but the substantive effect of rights review is often nullified behind the form. Accessibility_collapse (0.65) reflects that judicial remedies remain formally open but functionally collapse when parliament routinely signals willingness to override. Resistance (0.55) captures sustained judicial and civil-society contestation of the override power. Temporal measurements trace a trajectory from moderate post-enactment extraction to hardened majoritarian use over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary majority seat computes as low-directionality beneficiary: the constraint subsidizes its policy autonomy by removing judicial veto points. The judiciary and rights-bearing minorities compute as high-directionality targets: the same mechanism that coordinates majoritarian governance extracts interpretive finality and rights-security from them. The direct_democracy_advocates seat, though not a direct victim of extraction, is structurally excluded from the coordination story entirely. The engine will compute these divergent seat classifications from the same structural data; the reading does not resolve the divergence but makes it measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map directly to structural relationships: parliamentary_majority is named in beneficiaries because it captures the policy latitude and override capacity; judiciary and rights_bearing_minorities are named in victims because they bear the loss of final interpretive authority and contingent rights protection. The parliamentary majority's low directionality follows from beneficiary status plus institutional power and arbitrage-grade exit within the electoral cycle. The judiciary's high directionality follows from victim status plus constrained exit (it cannot resign en masse or escape its constitutional role). Rights-bearing minorities' high directionality is amplified by powerlessness and constrained exit (limited political leverage and high cost of emigration). No overrides are needed because the derivation chain produces accurate directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by preserving its genuine coordination function: it solves the real collective-action problem of aligning constitutional law with democratic accountability and prevents judicial oligarchy. Conversely, it prevents mislabeling as pure coordination (rope) by insisting on the declared victim set (judiciary, minorities) and the active enforcement required to sustain legislative override against judicial pushback. If the override power fell into disuse and became theatrical, the constraint would drift toward piton; if courts were abolished entirely, it would drift toward snare. The current measurement profile (rising extraction, stable enforcement, moderate theater) confirms tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_sovereignty_kernel_contest,
    'Is the legislative sovereignty reading a logically independent institutional choice, or does it presuppose and foreclose the popular sovereignty reading by absorbing constituent power into representative institutions?',
    'Comparative analysis of constitutional orders that mix parliamentary supremacy with direct democratic mechanisms (e.g., referenda on constitutional amendments) versus those that prohibit such mixing.',
    'If legislative sovereignty logically forecloses direct popular constitutional authority, the relation to popular_sovereignty_reading should be forecloses rather than influences; if they can coexist, the constraint''s classification may shift depending on which popular mechanisms are present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_sovereignty_kernel_contest, conceptual, 'Whether legislative sovereignty forecloses or merely influences popular sovereignty').

omega_variable(
    override_usage_pattern,
    'Does the legislative override power function primarily as a democratic safety valve for exceptional disagreements, or as a routine extraction mechanism targeting specific minorities?',
    'Empirical inventory of notwithstanding clause or override usage across jurisdictions: targets, frequency, and subsequent electoral consequences.',
    'If usage is rare and electorally costly, coordination function dominates and extractiveness is lower; if routine and targeted, extraction dominates and the constraint approaches snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_usage_pattern, empirical, 'Whether override usage is exceptional safety valve or routine extraction').

omega_variable(
    judiciary_residual_authority,
    'Do advisory courts under legislative supremacy retain sufficient informal interpretive influence to moderate their directionality, or is their authority fully extracted?',
    'Measure pre-legislative judicial consultation, informal compliance with advisory opinions, and legislative drafting patterns that anticipate judicial reaction despite override availability.',
    'If residual influence is high, the judiciary''s d value is lower than victim status suggests, and the constraint may recompute as less extractive; if override nullifies influence, the victim declaration holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_residual_authority, empirical, 'Whether advisory courts retain informal influence under legislative supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__legislative_sovereignty_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(cons_be_t50, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(cons_su_t50, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_text kernel, decomposed per the epsilon-invariance principle because the label 'constitutional supremacy' conflates structurally distinct claims about ultimate interpretive authority. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
