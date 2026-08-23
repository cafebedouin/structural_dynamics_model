% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Living Constitutionalism — Inherited Due Process Restraint
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the living constitutionalism reading
 *   of the Magna Carta kernel. It treats the 1215 charter not as a feudal
 *   settlement but as the founding instrument of an inherited constitutional
 *   restraint that binds all subsequent sovereign power through juridical
 *   precedent and evolutionary interpretation. The constraint coordinates the
 *   relationship between governed and governors by establishing that
 *   executive authority is always already limited by due process principles
 *   that evolve with circumstances but never disappear. The claimed type is
 *   rope: a coordination mechanism around inherited restraint with
 *   low-to-moderate extractiveness. The metric profile shows declining
 *   extractiveness and suppression from 1215 through the late 20th century as
 *   the constraint became internalized as constitutional culture, with a
 *   recent uptick (2024) reflecting renewed executive assertions of
 *   prerogative power in security and emergency contexts.
 *
 * KEY AGENTS:
 *   - subjects_citizens: Primary beneficiary (moderate/organized/constrained/national) — gains due process shield against arbitrary power
 *   - royal_prerogative: Primary victim (institutional/trapped/structural) — the claim of inherent executive authority is structurally constrained
 *   - executive_discretion: Victim (institutional/constrained/structural) — administrative discretion is bounded by evolving due process standards
 *   - judiciary: Agenda setter (institutional/biographical/arbitrage/national) — interprets and applies the evolutionary restraint
 *   - parliament_as_constrained_legislator: Beneficiary (powerful/biographical/constrained/national) — inherits legislative supremacy but bounded by charter principles
 *   - historical_barons: Excluded (powerless/trapped/local) — original parties to the 1215 compact, not in the contemporary conversation
 *   - constitutional_scholars: Observer (analytical/civilizational/analytical/universal) — analyze the interpretive tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Living Constitutionalism — Inherited Due Process Restraint").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '76faa5f6-cdeb-4acc-a910-5fd25e550ec6').
narrative_ontology:cs_kernel_codification('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', formalized).
narrative_ontology:cs_authority_grounding('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', lineage).
narrative_ontology:cs_interpretation_layer_present('76faa5f6-cdeb-4acc-a910-5fd25e550ec6').
narrative_ontology:cs_reading_relation('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', foundational, evolutionary_interpretation_legitimate).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', evolutionary_interpretation_legitimate, conventional).
narrative_ontology:cs_axiom('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', foundational, inherited_restraint_binds_sovereign).
narrative_ontology:cs_axiom_status(inherited_restraint_binds_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', inherited_restraint_binds_sovereign, conventional).
narrative_ontology:cs_reference_frame('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', charter_as_living_instrument).
narrative_ontology:cs_drift_state('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76faa5f6-cdeb-4acc-a910-5fd25e550ec6', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_as_constrained_legislator).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_as_constrained_legislator).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, due_process_hereditary_binding).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, evolutionary_interpretation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain due process protections against arbitrary arrest, detention, and property seizure. The constraint evolves to cover new rights (fair trial, privacy, non-discrimination) through judicial interpretation. Exit is constrained — they cannot opt out of the legal system, but they can emigrate or seek constitutional amendment. The benefit is diffuse but foundational: the rule of law itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens, beneficiary,
    moderate, biographical, constrained, national).

% The historical claim that the monarch possesses inherent executive authority not derived from statute. This claim is structurally constrained by the charter's evolutionary interpretation — every assertion of prerogative is measured against due process standards. The 'agent' is the institutional office of the Crown, not any individual monarch. It cannot exit the constraint without ceasing to be a constitutional monarchy.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative, payer,
    institutional, generational, trapped, national).

% The administrative state's discretionary powers — security classifications, emergency regulations, immigration enforcement, regulatory enforcement. These are bounded by evolving due process requirements (proportionality, procedural fairness, judicial review). Executive actors resist specific applications but generally accept the framework; exit would mean abandoning the rule-of-law legitimacy that makes their authority effective.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion, payer,
    institutional, biographical, constrained, national).

% Interprets and applies the evolutionary restraint. Gains institutional authority and legitimacy from being the charter's authoritative interpreter. Can 'arbitrage' by choosing interpretive methodologies (originalism, living constitutionalism, textualism) that shift the constraint's operation. Not a victim — the constraint is the source of their distinctive constitutional role.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter,
    institutional, biographical, arbitrage, national).

% Inherits legislative supremacy but finds it bounded by charter principles that courts enforce. Benefits: the constraint legitimizes parliamentary authority as 'constrained sovereignty' rather than raw power. Pays: cannot legislate contrary to evolving due process standards without constitutional amendment (politically difficult). Dual-positioned — both empowered and constrained by the same structure.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_as_constrained_legislator, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_as_constrained_legislator, payer).

% The 1215 parties to the charter — feudal magnates seeking specific relief from King John. Their compact was transformed into a universal constitutional principle they did not authorize and cannot control. They are structurally excluded from the living tradition; their feudal grievances are not the constraint's current coordination function.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, historical_barons, excluded,
    powerless, immediate, trapped, local).

% Analyze the interpretive tradition, trace doctrinal genealogy, and evaluate competing readings. Neither collect nor pay; their role is to make the constraint's structure visible. Their exit is analytical — they can change frameworks but not the constraint itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, evolving framework that coordinates the relationship between sovereign power and the governed by establishing that all executive authority is limited by due process principles. Solves the problem of arbitrary rule without requiring constant renegotiation or revolution — the constraint itself generates the interpretive mechanism for its own adaptation.
% TRANSFER_FUNCTION: Transfers legitimacy from raw power to law-constrained authority. The executive surrenders the claim of inherent unlimited discretion; in return, it gains recognized legitimate authority bounded by law. Subjects surrender the possibility of revolutionary overthrow as the only check on power; they gain reliable due process protections. The judiciary receives interpretive authority as the constraint's institutional guardian.
% ABSENT_VOICES: The historical barons (original parties) are excluded — their feudal compact was universalized without their consent. Colonized peoples were excluded from the charter's protections for centuries; their inclusion came through struggle, not the constraint's internal logic. Future generations are absent but structurally affected — the constraint's evolutionary interpretation binds them to interpretive choices made today.
% DISAPPEARANCE_RATIONALE: If the living constitutionalism reading vanished overnight, the UK/Commonwealth constitutional order would lose its foundational due process anchor. Executive power would revert to parliamentary sovereignty as the sole restraint (the sibling reading), judicial review would lose its charter grounding, and the evolutionary interpretation mechanism would cease. The world would rearrange toward either parliamentary supremacy or a codified constitution — not stay the same.
% FOUNDING_PROBLEM: Arbitrary executive power — the monarch's ability to arrest, tax, and adjudicate without lawful process or consent. The 1215 charter addressed this by establishing that the king is subject to law, not above it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (arbitrary executive power) is attested as live by: (1) the UK Supreme Court in Miller II/Cherry (2019) citing Magna Carta Chapter 29 as binding on prime ministerial prerogative; (2) the European Court of Human Rights tracing Article 6 fair trial rights to Magna Carta's due process lineage; (3) constitutional scholars across the common law world (e.g., Dicey, Bingham, Waldron) who identify executive overreach as a persistent structural problem. No corroboration comes solely from the constraint's beneficiaries — the attestation spans judicial, scholarly, and international institutional seats.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28 at interval end) because the constraint primarily coordinates — it provides a stable framework for limiting power that all parties (eventually) rely on. The 1215 peak (0.35) reflects the charter's initial imposition on a reluctant monarch; the long decline tracks the internalization of due process as constitutional culture. The 2024 uptick to 0.28 captures post-9/11 executive claims of inherent authority in national security that test the restraint's boundaries. Suppression follows a similar arc: high initially (0.65) when enforcement required baronial armies and papal annulment, falling as the constraint became self-enforcing through judicial review and political culture. Theater ratio is lowest mid-century (0.06-0.07) when the constraint's coordination function was most genuine and least performative; the recent rise to 0.12 reflects ceremonial invocations of Magna Carta in political rhetoric that exceed its operative legal force. Accessibility collapse (0.35) is moderate: alternatives (absolute monarchy, parliamentary supremacy without charter limits) remain conceptually available but politically inaccessible. Resistance (0.45) is moderate: executive branches periodically contest the scope of due process but rarely the principle itself.
 *
 * PERSPECTIVAL GAP:
 *   From the subject/citizen seat, the constraint appears as mountain-like protection — a near-natural law of due process. From the executive seat, it appears as a rope that coordinates legitimate authority but extracts compliance costs. From the judicial seat, it is the agenda-setting framework that gives interpretive authority its legitimacy. The engine computes these per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects/citizens are structural beneficiaries (d ~ 0.2): they collect the due process shield without administering it. The judiciary and parliament are agenda-setters with some beneficiary characteristics (d ~ 0.3-0.4): they wield interpretive/legislative authority but are also constrained by it. Royal prerogative and executive discretion are structural victims (d ~ 0.7-0.8): they bear the asymmetric extraction of having their claimed inherent authority bounded. Historical barons are excluded (trapped, no exit) — their feudal compact was transformed into a universal constitutional principle they did not authorize. Constitutional scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary executive power) remains live but transformed: 1215's arbitrary royal power becomes 2024's executive emergency powers, surveillance authority, and administrative state discretion. The constraint has not suffered mandatrophy — its coordination function (limiting arbitrary power through law) remains essential — but its extractiveness on executive discretion has accumulated in recent decades as the administrative state expanded. The constraint is not a piton: its theater ratio remains low and its function is actively maintained through judicial review, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''magna_carta_constraint_authority'', and does the living constitutionalism reading instantiate a structurally distinct constraint from its siblings?',
    'Compare ε, beneficiary/victim sets, and claimed_type across the three declared readings (feudal_obsolescence_reading, parliamentary_sovereignty_reading, living_constitutionalism_reading). If they differ structurally, the kernel decomposes into multiple constraints per ε-invariance.',
    'Confirms this story correctly isolates one reading as a clean ε-invariant constraint. If readings share ε and structural data, the decomposition was unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame kernel/reading identity for this constraint story').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the living constitutionalism reading''s core premise (evolutionary interpretation binds all subsequent rulers) logically foreclose the parliamentary sovereignty reading (Parliament can revise/repeal any charter provision) within a single framework?',
    'Analyze whether a single legal framework can simultaneously hold that (a) charter provisions bind through evolutionary interpretation and (b) Parliament retains unlimited revisionary power over those same provisions. If mutually exclusive, relation = forecloses; if held by different parties simultaneously, relation = coexists_with.',
    'Determines cs_structure.reading_relations entry for parliamentary_sovereignty_reading. A forecloses relation would be rare structural evidence of genuine logical incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether living constitutionalism and parliamentary sovereignty are logically incompatible in one framework').

omega_variable(
    executive_discretion_as_victim,
    'Is ''executive_discretion'' a genuine victim (bears asymmetric extraction) or a vindicated proposition (the constraint''s operation vindicates limited executive power)?',
    'Track whether executive actors experience the constraint as costly extraction they resist, or as a coordination benefit that legitimizes their authority. Structural victimhood requires active resistance and exit-seeking behavior.',
    'If executive_discretion is a vindicated proposition rather than a victim, the victim set shrinks to royal_prerogative only, altering the coordination/extraction balance and potentially shifting claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_discretion_as_victim, empirical, 'Whether executive power bears extraction or is vindicated by the constraint').

omega_variable(
    historical_continuity_vs_reinvention,
    'Does the living constitutionalism reading describe genuine historical continuity of restraint, or a repeated reinvention that cites Magna Carta as legitimating cover?',
    'Trace citation networks and doctrinal genealogy: when courts invoke Magna Carta, are they applying a continuous interpretive tradition or selectively reviving dormant language for contemporary justification?',
    'If reinvention, the constraint''s coordination function is performative (higher theater_ratio) and its extractiveness on executive power may be underestimated. If genuine continuity, the low extractiveness reflects real coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_continuity_vs_reinvention, empirical, 'Whether evolutionary interpretation is continuous tradition or periodic reinvention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.45).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1297, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1297, 0.38).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1628, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1628, 0.25).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1689, 0.15).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1765, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1765, 0.12).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1832, 0.1).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1911, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1911, 0.08).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1945, 0.07).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t1998, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1998, 0.06).
narrative_ontology:measurement(magna_carta_living_constitutionalism_tr_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.35).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1297, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1297, 0.32).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1628, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1628, 0.28).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1689, 0.22).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1765, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1765, 0.25).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1832, 0.2).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1911, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1911, 0.18).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t1998, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1998, 0.12).
narrative_ontology:measurement(magna_carta_living_constitutionalism_be_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.65).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1297, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1297, 0.45).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1628, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1628, 0.35).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1689, 0.2).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1765, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1765, 0.15).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1832, 0.12).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1911, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1911, 0.1).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t1998, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1998, 0.06).
narrative_ontology:measurement(magna_carta_living_constitutionalism_su_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'Magna Carta authority' label into three structurally distinct readings per ε-invariance. Living constitutionalism (this story) claims rope with ε≈0.28, victims={royal_prerogative, executive_discretion}, beneficiaries={subjects_citizens, judiciary, parliament}. Feudal obsolescence claims mountain/piton with ε≈0.05, no living victims. Parliamentary sovereignty claims scaffold/tangled_rope with ε≈0.35, victims={judicial_review}, beneficiaries={parliament}. The readings are linked via affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__living_constitutionalism_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
