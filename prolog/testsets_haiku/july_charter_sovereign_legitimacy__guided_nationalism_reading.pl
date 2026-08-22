% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: Islamic-Nationalist Charter Sovereignty Framework (Guided Nationalism Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary charter enshrines religious identity as the
 *   constitutional ground of state legitimacy. The Charter presents this
 *   framework as necessary for national unification after revolution; secular
 *   civil society and religious minorities experience it as institutional
 *   extraction—transfer of legal authority from secular to religious
 *   institutions, suppression of secular law-making, and exclusion of
 *   non-religious identity frames from constitutional standing. This
 *   constraint captures ONE reading of the contested charter kernel: the
 *   guided-nationalism reading, which emphasizes religious identity as a
 *   sovereign legitimacy ground. Two sibling readings exist: the
 *   military-custodian reading (Charter ratifies military permanence) and the
 *   secular-democratic reading (Charter mandates secular institutions). This
 *   story instantiates the nationalist reading only, authoring its ε
 *   independently of its siblings.
 *
 * KEY AGENTS:
 *   - Religious-nationalist elite: institutional power, agenda-setter role, administers the religious-identity ground through courts and legislatures
 *   - Secular civil society: moderate power, payer role, constrained exit, loses institutional voice on secular law-making
 *   - Religious minorities: powerless, trapped, bears exclusion from constitutional legitimacy
 *   - Military institution: institutional power, hybrid beneficiary/agenda-setter, uses religious-nationalist frame for stability but retains strategic flexibility
 *   - Traditional religious authorities: organized power, beneficiary, gain formal constitutional standing and material resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "Islamic-Nationalist Charter Sovereignty Framework (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '5ef66232-32fb-442e-a004-a9dbaea878a9').
narrative_ontology:cs_kernel_codification('5ef66232-32fb-442e-a004-a9dbaea878a9', fixed_text).
narrative_ontology:cs_authority_grounding('5ef66232-32fb-442e-a004-a9dbaea878a9', extraction).
narrative_ontology:cs_interpretation_layer_present('5ef66232-32fb-442e-a004-a9dbaea878a9').
narrative_ontology:cs_reading_relation('5ef66232-32fb-442e-a004-a9dbaea878a9', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('5ef66232-32fb-442e-a004-a9dbaea878a9', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('5ef66232-32fb-442e-a004-a9dbaea878a9', foundational, religious_identity_legitimate_constitutional_ground).
narrative_ontology:cs_axiom_status(religious_identity_legitimate_constitutional_ground, holdable).
narrative_ontology:cs_axiom_grounding('5ef66232-32fb-442e-a004-a9dbaea878a9', religious_identity_legitimate_constitutional_ground, deontological).
narrative_ontology:cs_axiom('5ef66232-32fb-442e-a004-a9dbaea878a9', secondary, secular_law_subordinate_to_religious_constitutional_provisions).
narrative_ontology:cs_axiom_status(secular_law_subordinate_to_religious_constitutional_provisions, holdable).
narrative_ontology:cs_axiom_grounding('5ef66232-32fb-442e-a004-a9dbaea878a9', secular_law_subordinate_to_religious_constitutional_provisions, conventional).
narrative_ontology:cs_reference_frame('5ef66232-32fb-442e-a004-a9dbaea878a9', post_revolutionary_unified_nationalist_state).
narrative_ontology:cs_drift_state('5ef66232-32fb-442e-a004-a9dbaea878a9', contemporary_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5ef66232-32fb-442e-a004-a9dbaea878a9', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_elite).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, leftist_intellectuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, traditional_religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the constitutional framework that places Islamic identity and religious law at the center of national legitimacy. Interprets the Charter as the mechanism through which religious nationalism becomes state doctrine. Their exit from this position would require abandoning the core claim that the nation's identity IS fundamentally religious — identity-fused with the institutional role. They administer the framework through courts, legislatures, and religious councils.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_elite, agenda_setter,
    institutional, generational, identity_locked, national).

% Experiences the Charter as constraining secular law-making, public institutions, and individual freedoms on religious grounds. Lawyers, academics, and artists find their professional scope narrowed by religious constitutional provisions. They can advocate for reform but face institutional barriers (courts staffed by religious-nationalist judges) and social sanction. Exit means emigration or enforced silence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, constrained, national).

% Bear the constraint most directly: the Charter's religious identity ground privileges certain faiths or interpretations over others. Their legal status, property rights, worship freedoms, and education are shaped by religious provisions written by and for a majority group. They have no effective exit within the nation and minimal institutional voice in interpretation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Shaped by the constraint through ideological identity-lock: their professional identity as secular, rationalist thinkers is fundamentally at odds with a state that grounds legitimacy in religious identity. Many face professional marginalization, publishing bans, or surveillance. Their exit would require abandoning core identity commitments (secular intellectual tradition).
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, leftist_intellectuals, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, leftist_intellectuals, excluded).

% Benefits from the Charter's religious-nationalist framing as a vehicle for institutional stability and national unity. The military frames itself as guarantor of both nationalism and order. Can shift its framing or form coalitions with religious elites to maintain power, giving it more exit flexibility than the purely religious-nationalist seat.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_institution, agenda_setter).

% Gain formal constitutional standing and influence over law-making through the Charter's religious-identity ground. Their interpretive authority becomes state authority. They benefit materially (funding, institutional recognition) and ideologically (their theology becomes constitutional doctrine). Exit would mean losing this institutional power.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, traditional_religious_authorities, beneficiary,
    organized, generational, constrained, national).

% Structurally absent from the Charter's drafting and interpretation. They would argue for secular constitutional principles, civil-law supremacy, and individual rights decoupled from religious identity. Their presence would reframe the legitimacy ground entirely. They remain institutionally excluded.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_democratic_advocates, excluded,
    moderate, biographical, constrained, national).

% Document the Charter's operation and its effects on religious minorities and secular groups. They generate external accountability pressure but have no formal veto over the constraint's operation. They provide data that feeds into mandatrophy questions (does the founding problem of national coherence still exist, or has the arrangement shifted to pure religious dominance).
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_elite).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the post-revolutionary nation around a core identity narrative: religious nationalism provides a single legitimacy ground that avoids the fragmentation that would result from purely secular or military rule alone. It creates a shared frame for law-making and public authority that majority groups accept as legitimate.
% TRANSFER_FUNCTION: Moves cultural, legal, and institutional authority from secular institutions to religious ones. Secular lawyers, academics, and civil-rights advocates lose say over constitutional interpretation; religious scholars gain formal power. Power over education, family law, personal status, and public morality shifts toward religious authorities. Material resources (state funding, institutional positions) flow to religious organizations.
% ABSENT_VOICES: Secular democratic advocates and religious minorities who would argue for individual rights decoupled from religious identity, or for separation of religious authority and state power, are structurally excluded from Charter framing. They can mobilize outside the constitutional structure but have no seat at its authoring or interpretation. Their absence enabled the Charter to be adopted without consensus.
% DISAPPEARANCE_RATIONALE: If the Charter's religious-identity legitimacy ground vanished overnight, secular law would resume supremacy, religious minorities would gain protection, and the military/religious-elite coalition would lose its foundational justification. The state's institutional structure would reorient toward secular administration or democratic contestation; the rearrangement would be immediate and structural.
% FOUNDING_PROBLEM: Post-revolutionary state required a single unifying legitimacy ground to prevent institutional fragmentation and ethno-sectarian competition. Religious identity (shared by a majority) offered that ground, appearing to transcend class and military factionalism.
% FOUNDING_PROBLEM_CORROBORATION: Religious-nationalist elites attest the founding problem is live: national unity still depends on religious identity as the legitimacy anchor. Secular civil society and religious minorities attest the problem was already solved by the revolution itself and the constraint persists as religious dominance masquerading as unification. International observers and academic analysis (from outside the benefiting parties) support the shifted-function reading: the founding unification problem has become a tool for suppressing dissent.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) and rising: the constraint begins as coordination (shared identity ground for post-revolutionary unity) but increasingly operates as extraction as secular alternatives are institutionally suppressed and religious authority accumulates power. Suppression is high (0.71) and rising sharply in the early interval—the constraint's persistence depends on actively excluding secular law and constraining minority voice. Theater rises early (to ~0.26 by year 5) and plateaus (0.42 by year 30): initially performative national-unity discourse masks emerging religious-institutional dominance; later the performative ratio stabilizes as the extraction becomes normalized. The measurement series are on one shared time grid; every metric is authored at every point. The rising suppression_requirement in early years reflects the enforcement machinery built to enforce religious constitutional provisions and exclude secular alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The religious-nationalist elite perceive the Charter as genuine coordination enabling national coherence; secular and minority seats perceive it as enforced extraction of legal authority and identity dominance. The engine computes this divergence from power, exit_options, and beneficiary/victim declarations. Religious nationalists have identity_locked exit (abandoning the reading requires abandoning core identity commitment); secular groups have constrained/trapped exit (institutional barriers and social sanction prevent leaving without material loss). This structural asymmetry drives the per-seat classification divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious-nationalist elite: d near 0.0 (full beneficiary — they gain institutional power, authority, identity validation). Secular civil society: d near 0.8 (full target — lose institutional voice, constrained exit prevents arbitrage). Religious minorities: d = 1.0 (pure target — trapped exit, no institutional standing, legal status subordinated). Military institution: d ~0.4 (moderate beneficiary — gains stability justification but retains strategic flexibility; can shift coalitions). Traditional religious authorities: d near 0.1 (beneficiary — gain power without running the state). The directional asymmetry reflects the core extraction: the constraint transfers authority FROM secular institutions TO religious ones, creating a clear payer/beneficiary split.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-revolutionary unification) is contested. Religious-nationalist elites attest it is live—national coherence still depends on shared religious identity. Secular civil society and minorities attest it is dead—the revolution itself created sufficient unity; the constraint persists as religious-institutional power accumulation. The measurement series support the shifted-function reading: extraction rises from 0.48 (early unification stage) to 0.68 (stabilized religious dominance); suppression requirement rises sharply in early years (enforcing exclusion of secular alternatives) and plateaus (enforcement become routine); theater ratio rises and plateaus (unification discourse loses functional role, becomes normalization backdrop). The combination suggests mandatrophy: the Charter's justification (unification) has outlived its necessity; the arrangement persists through active enforcement and identity fusion rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Did the post-revolutionary unification problem persist beyond the Charter''s early adoption, or was national coherence already achieved by revolution itself?',
    'Comparative institutional analysis of state stability, conflict incidence, and public support during early vs. later Charter periods. Survey data on whether citizens report the religious-identity ground as necessary for national unity or as imposed elite preference.',
    'If the problem was already solved, the Charter qualifies as mandatrophic—a coordination frame that has become a vehicle for extraction. The constraint would shift from tangled_rope toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding unification problem remained live or became a cover story for religious institutional power.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the guided-nationalism reading logically foreclose the secular-democratic reading within a single institutional framework, or do they coexist as competing framings held by different coalitions?',
    'Constitutional analysis: can the Charter be interpreted to support both religious-nationalist legitimacy AND secular-democratic principles simultaneously, or are the core premises inherently contradictory?',
    'If forecloses: the readings are mutually exclusive; one will dominate as institutional power settles. If coexists_with: both readings remain live options for different parties within the same constitutional structure. Classification of the reading_relations in cs_structure depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the nationalist and democratic readings are logically incompatible or can coexist within the Charter.').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the suppression of secular alternatives primarily structural (institutional barriers, courts staffed with religious-nationalist judges) or internalized (secular actors have absorbed the religious-nationalist frame as legitimate, making exit unthinkable)?',
    'Post-constraint analysis: if secular civil society is suddenly given institutional voice (new courts, constitutional amendment), does secular law advocacy resume immediately, or has the constraint created internalized acceptance of religious legitimacy grounds?',
    'If structural, removing institutional barriers would enable rapid secular institutional recovery. If internalized, the constraint''s effective suppression is higher than measured; victims would carry the suppression with them even after formal removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Suppression mechanism: structural institutional barriers vs. internalized legitimacy frame.').

omega_variable(
    religious_minorities_exit_options_precision,
    'Are religious minorities trapped by the constraint alone, or are they trapped by broader national structures (citizenship laws, property rights) independent of the Charter''s religious-identity ground?',
    'Comparative analysis: in jurisdictions with similar demographic composition but secular constitutions, what exit options do minority religious groups possess?',
    'If trapped by Charter alone, removing the religious-identity legitimacy ground would substantially improve their position. If trapped by broader structures, Charter reform alone would not restore exit options. Directionality precision depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_minorities_exit_options_precision, empirical, 'Whether religious minorities'' trapped status is specific to the Charter or structural to national citizenship.').

omega_variable(
    kernel_contest_sibling_relations_ambiguity,
    'What is the structural relationship between the guided-nationalism reading (this constraint) and its sibling military-custodian reading: does nationalism foreclose military custodianship, or do they coexist as compatible institutional framings?',
    'Constitutional and empirical analysis: can a Charter simultaneously ground legitimacy in religious-nationalist identity AND establish permanent military guardianship, or does military custodianship require a depoliticized (secular or technocratic) legitimacy frame?',
    'If nationalism forecloses custodianship: reading_relations should list military-custodian as ''forecloses''. If they coexist: ''coexists_with''. If nationalism creates structural pressure on custodianship: ''influences''. The relation choice affects how the engine evaluates constraint-family stability and mutation risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_sibling_relations_ambiguity, conceptual, 'Whether guided-nationalism and military-custodian readings logically coexist or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(july_tr_t5, observed).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(july_tr_t10, observed).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(july_tr_t15, observed).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(july_tr_t20, observed).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(july_tr_t25, observed).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(july_tr_t30, observed).
narrative_ontology:measurement(july_tr_t35, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(july_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(july_be_t5, observed).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(july_be_t10, observed).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(july_be_t15, observed).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(july_be_t20, observed).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(july_be_t25, observed).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(july_be_t30, observed).
narrative_ontology:measurement(july_be_t35, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(july_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(july_su_t5, observed).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(july_su_t10, observed).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement_basis(july_su_t15, observed).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(july_su_t20, observed).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(july_su_t25, observed).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(july_su_t30, observed).
narrative_ontology:measurement(july_su_t35, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(july_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.15).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% The july_charter_sovereign_legitimacy kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of the same written charter. This story (guided-nationalism reading) treats the charter as establishing religious identity as the sovereign legitimacy ground. The secular-democratic reading treats the same charter as mandating secular democratic institutions; the military-custodian reading treats it as establishing permanent military guardianship. Each reading has its own ε, beneficiary/victim structure, and cs_structure fields. Links via network.affects_constraints connect the family members. The kernel is the charter-as-written; the readings are competing institutional framings of what it legitimates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
