% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Basic Law Interpretive Authority
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint instantiates the PARLIAMENTARY SOVEREIGNTY READING of
 *   Israel's constitutional authority structure. Under this reading, the
 *   Knesset—as the elected representative body—retains supreme authority to
 *   interpret and amend the Basic Laws via simple majority vote. The
 *   judiciary may offer advisory opinions, but the Knesset is not bound by
 *   judicial constitutional review; the legislature can override any judicial
 *   position through new legislation that redefines the constitutional
 *   boundary itself. This reading places majoritarian electoral
 *   accountability above institutional separation of powers. It is one of
 *   three live readings of the same kernel (basic_law_interpretive_boundary);
 *   the sibling readings—judicial supremacy and balanced
 *   contestation—instantiate structurally different constraints with
 *   different ε values, beneficiary structures, and stakeholder situations.
 *
 * KEY AGENTS:
 *   - Knesset majority coalition: sets and enforces the interpretive agenda; authority is unchecked except by electoral cycles and treaty obligations.
 *   - Supreme Court justices: occupy an advisory or subordinate position; their constitutional pronouncements lack binding force over legislative will.
 *   - Opposition parties: lack the majority votes to amend Basic Laws; must contest via electoral channels, not judicial veto.
 *   - Minority-rights holders: protected by statutory/coalition accommodation only, not by judicially-enforceable constitutional entrenchment.
 *   - Electoral public: holds ultimate authority through elections; constitutional choices are directly enforceable.
 *   - International treaty bodies: formally bound by Israel's commitments but lack direct enforcement mechanism under this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.15).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.22).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Basic Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'c2afde36-2027-44c5-9be6-09f0514df278').
narrative_ontology:cs_kernel_codification('c2afde36-2027-44c5-9be6-09f0514df278', formalized).
narrative_ontology:cs_authority_grounding('c2afde36-2027-44c5-9be6-09f0514df278', lineage).
narrative_ontology:cs_interpretation_layer_present('c2afde36-2027-44c5-9be6-09f0514df278').
narrative_ontology:cs_reading_relation('c2afde36-2027-44c5-9be6-09f0514df278', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2afde36-2027-44c5-9be6-09f0514df278', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('c2afde36-2027-44c5-9be6-09f0514df278', foundational, parliamentary_majoritarian_sovereignty).
narrative_ontology:cs_axiom_status(parliamentary_majoritarian_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c2afde36-2027-44c5-9be6-09f0514df278', parliamentary_majoritarian_sovereignty, deontological).
narrative_ontology:cs_axiom('c2afde36-2027-44c5-9be6-09f0514df278', foundational, judicial_review_subordinate_to_legislative_will).
narrative_ontology:cs_axiom_status(judicial_review_subordinate_to_legislative_will, holdable).
narrative_ontology:cs_axiom_grounding('c2afde36-2027-44c5-9be6-09f0514df278', judicial_review_subordinate_to_legislative_will, deontological).
narrative_ontology:cs_reference_frame('c2afde36-2027-44c5-9be6-09f0514df278', knesset_constitutional_supremacy).
narrative_ontology:cs_drift_state('c2afde36-2027-44c5-9be6-09f0514df278', contemporary_post_2023_judicial_reform_struggle, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c2afde36-2027-44c5-9be6-09f0514df278', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_public).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, political_opposition_parties).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_advocacy_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_rights_holders).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The elected parliamentary majority interprets and amends Basic Laws directly via legislation, with no judicial veto power over substantive policy choices. The coalition sets the interpretive agenda through legislative amendment; dissenting justices advise but do not constrain. Exit for this seat: losing elections or internal coalition defection, but legislative supremacy persists across electoral cycles under this reading.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Judicial review authority is advisory or subordinate: justices may issue interpretive opinions, but the Knesset retains authority to override by simple amendment or clarifying legislation. The court's pronouncements on constitutionality are not binding constraints on legislative will. Exit: resignation in protest, or accepting the subordinate institutional role.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices, observer).

% Lack majority power to amend Basic Laws; their statutory or constitutional objections to majority legislation are subject to override by coalition vote. The constraint denies them a judicial veto path; they must contest policies through electoral channels or legislative minority rights only.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, political_opposition_parties, payer,
    powerful, biographical, constrained, national).

% Cannot reliably invoke constitutional constraints on legislation; the Knesset majority retains power to redefine the constitutional boundary itself. Advocacy relies on electoral persuasion, coalition-building, or international pressure, not judicial nullification.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_advocacy_coalitions, payer,
    organized, biographical, constrained, national).

% Israel's treaty commitments (human rights, humanitarian law, trade) are stated as constraints the Knesset remains bound to honor, but under this reading the Knesset retains unilateral power to interpret or modify them via new legislation. International bodies have no direct enforcement mechanism within Israeli constitutional law; exit: treaty withdrawal (formally available but politically costly).
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, beneficiary,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, excluded).

% Retains ultimate authority through elections: the constraint makes the Knesset directly accountable to electoral majorities, unfiltered by judicial gatekeeping. The public's constitutional choices are enforceable through voting; no constitutional court can overrule the people via the legislature.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_public, beneficiary,
    organized, biographical, mobile, national).

% Are subject to majority-amended Basic Laws with no judicial override available. Their protection depends on coalition-building or statutory accommodation, not constitutional entrenchment via judicial review. Exit paths are limited: emigration (high cost) or acceptance of majoritarian authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_rights_holders, payer,
    powerless, biographical, identity_locked, national).

% Assess whether this reading represents genuine parliamentary democracy or a constraint-on-constraint (majoritarian tyranny risk) depending on external perspective and values. They produce analysis but hold no enforcement power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, democratic_theorists_and_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified locus of constitutional authority—the Knesset—that can adapt and reinterpret the Basic Law framework in response to changing circumstances without requiring supramajority consensus or external judicial validation. Solves the problem of constitutional gridlock by vesting amendment power in an elected body accountable through regular elections.
% TRANSFER_FUNCTION: Transfers authoritative constitutional interpretation power from the judiciary (advisory or subordinate status) to the legislature (supreme and binding status). The arrangement also transfers the cost of constitutional revision from a supermajority or consensus standard to a simple parliamentary majority.
% ABSENT_VOICES: International human-rights bodies and the global judiciary are structurally absent from the domestic interpretive frame; they would argue for supra-legislative constitutional constraints and independent judicial review, but this reading explicitly denies them standing. Judicial dissenters and minority-rights advocates would object to the removal of their veto if they were the decision-making party; under this reading they remain voices in the chamber but not veto-holders.
% DISAPPEARANCE_RATIONALE: If parliamentary-supremacy-over-constitution disappeared and were replaced by judicially-enforceable constitutional limits, the Knesset would lose its power to unilaterally redefine constitutional boundaries. Policy-making would reorganize around judicial gatekeeping; some legislation the current coalition can pass would be invalidated; the balance of power between institutions would shift toward the court. The entire Israeli constitutional regime would reorganize.
% FOUNDING_PROBLEM: The absence of a constitutive moment or written single-document constitution meant there was no agreed external standard above the Knesset to which both the legislature and judiciary could appeal. The founding problem was: who decides what the Basic Laws mean—the elected representatives or appointed judges? Under this reading, the answer was: the elected representatives, because they hold democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: This reading is defended by parliamentary democracy theorists (e.g., Yaniv Roznai on majoritarian constitutionalism), some Israeli legal scholars (e.g., Professors Gavison, Kamir on parliamentary sovereignty), and practiced through the Knesset's legislative behavior—but it is CONTESTED by the Supreme Court's counter-reading (judicial supremacy) and by international human-rights bodies and comparative constitutional scholars who argue the Court has enforced constitutional limits. No universal agreement exists; the corroboration is domestic (parliamentary practice) and international dissent is documented.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15) because this reading asserts that no party GAINS from the constraint itself—rather, the constraint articulates a principle (majoritarian constitutional authority) that distributes power equally to all electoral majorities in turn. The coalition that holds power today benefits NOW, but loses that benefit if defeated at the next election. Suppression is also low (0.22) because the constraint operates via transparent institutional procedure (parliamentary amendment) rather than hidden coercion; alternatives (judicial supremacy, balanced contestation) are openly advocated and debated. Theater ratio is low-to-moderate (0.18) because the Knesset's constitutional amendment power is genuine and regularly exercised, though increasingly accompanied by rhetorical defense against the rival judicial supremacy reading. The measurement trajectory shows rising extractiveness, theater, and suppression from 1992–2023, reflecting the growing institutional contest with the Court and the 2023–2024 judicial reform debate (where the reading's stakes became explicit and suppression increased). The 2026 projections reflect uncertainty about the outcome of that contest. The shared time grid spans the interval 1992 (Basic Law: Human Dignity and Liberty) to 2026.
 *
 * PERSPECTIVAL GAP:
 *   The Knesset majority coalition's seat perceives this reading as genuine democratic principle with minimal extraction—they view it as 'the people's will, unchecked.' The Supreme Court's seat perceives it as a constraint that removes the Court's constitutionally-protected independence and makes the judiciary subordinate to temporary legislative majorities—extractive from the institutional perspective. Minority-rights holders perceive it as exposing them to majoritarian override without remedy. International observers perceive it as risking human-rights violations unconstrained by judicial review. The engine computes these divergent classifications from the structural data (power, exit_options, role, situation) without the commentary reconciling them—the perspectival gap IS the measurement the divergence enables.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority coalition has high institutional power and near-arbitrage exit (losing elections is temporary; a new coalition still inherits the same supremacy principle). Directionality for this seat is near 0.0 (full beneficiary of the constraint—it affirms their authority). The Supreme Court justices are powerful but constrained exit (resignation is costly; accepting subordinate status persists across electoral cycles). Directionality approaches 0.4–0.5 (symmetric pain and benefit: they lose institutional supremacy but retain advisory authority). Minority-rights holders are powerless and identity-locked (their status and rights as minorities are unchangeable; exit via emigration is high-cost). Directionality for this seat approaches 1.0 (full target—the constraint exposes them to override). The opposition parties are powerful but constrained (they cannot amend Basic Laws without the majority; they must contest electorally or via international pressure). Directionality approaches 0.6–0.7 (targets, because their policy options are structurally limited). No directionality overrides are needed; the structural derivation from beneficiary/victim + exit produces reasonable seat-specific d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem—'who decides what the Basic Laws mean?'—remains LIVE and CONTESTED. The Knesset's answer ('we do, the elected representatives') is asserted and defended, but it is actively contested by the Supreme Court (judicial supremacy reading), civil society, and international bodies. The problem has not aged into a dead or obsolete category. Mandatrophy does NOT apply. The constraint is functionally alive—it is the object of ongoing institutional struggle. Theater ratio rising toward 0.19 (2023) reflects the struggle becoming increasingly explicit and performative (parliamentary debates about judicial reform, constitutional rhetoric), but theater is not yet dominant (theater_ratio < 0.5); functionality persists underneath. The rising suppression_requirement (up to 0.25 at 2023) signals that the constraint's maintenance has begun to require active institutional effort to suppress the competing judicial supremacy reading—this is the kernel contest making itself visible in institutional behavior. Mandatrophy resolution: not applicable. The constraint is neither mandatorily resolved nor a candidate for resolution; it remains contested and live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_foreclosure,
    'Does the parliamentary sovereignty reading logically foreclose the judicial supremacy reading within a single constitutional framework, or do they coexist as live positions held by different institutional actors?',
    'Examine the logical structure of each reading''s core premise: does one deny a core premise the other asserts? Or do they occupy different institutional seats and compete through practice? If competition is through practice/politics rather than logical negation, they coexist; if one denies the other''s core axiom, foreclosure is present.',
    'If foreclosure: the readings cannot both be true within one constitutional order; the kernel contest is zero-sum and one reading must win to establish constitutional stability. If coexistence: both readings can persist in different institutional seats; constitutional ambiguity persists (the actual Israeli state of affairs circa 2024).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether parliamentary sovereignty and judicial supremacy are logically incompatible or practically coexistent readings of the same kernel.').

omega_variable(
    electoral_majority_vs_constitutional_entrenched_rights,
    'Can rights and protections be constitutionally entrenched against electoral majorities while simultaneously affirming that electoral majorities hold ultimate constitutional authority?',
    'Analyze whether the constraint''s claim (Knesset majority has ultimate authority) is logically compatible with the existence of unamendable provisions or judicially-enforced ceilings on majority power. If minority rights are protected from majority vote, then the majority does not hold ultimate authority over that domain.',
    'If incompatible: the constraint''s low extractiveness (0.15) assumes minorities can be overridden, making the constraint more extractive for powerless identity-locked agents than authored. If compatible: a reconciliation mechanism must be specified (e.g., international treaty obligations act as external constraint on the majority without requiring judicial supremacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_majority_vs_constitutional_entrenched_rights, conceptual, 'The logical compatibility of majoritarian constitutional authority with entrenched minority protections.').

omega_variable(
    suppression_mechanism_escalation,
    'Is the rising suppression_requirement (from 0.10 in 1992 to 0.25 in 2023) a structural feature of maintaining parliamentary sovereignty against institutional resistance, or does it signal that the reading is losing factual fit with Israeli constitutional practice?',
    'Distinguish between (A) active suppression of the competing judicial supremacy reading by parliamentary institutional effort (legislative reform, political pressure on Court appointments) and (B) institutional recognition that the reading is no longer descriptively accurate and active suppression is required to prop up a faltering claim.',
    'If (A): the escalating suppression is the cost of maintaining the reading''s supremacy against institutional resistance; the reading remains structurally coherent but politically contested. If (B): the reading has begun to decay as a live constitutional arrangement and the engine should flag it as a candidate for reclassification or piton dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_escalation, empirical, 'Whether rising suppression reflects active defense of the reading''s supremacy or loss of its factual purchase on Israeli constitutional practice.').

omega_variable(
    international_treaty_constraint_boundary,
    'Do international treaty obligations (human rights covenants, humanitarian law) constitute a real external limit on the Knesset''s majoritarian authority under this reading, or are they subordinate to the Knesset''s interpretive will?',
    'Examine whether the Knesset treats international treaty commitments as binding law it cannot unilaterally reinterpret, or as commitments it can modify via new legislation that reinterprets the treaty''s scope. If the former, an external constraint on majoritarian authority exists; if the latter, the constraint is purely internal and this reading''s ε remains low.',
    'If treaties are binding: an external veto on Knesset power exists (via international dispute mechanisms); the reading''s unconstrained-sovereignty claim is overstated and ε should rise to reflect this external limit. If treaties are subject to unilateral Knesset reinterpretation: parliamentary sovereignty is truly unconstrained domestically, and ε remains low (constrained only by international diplomatic pressure, not law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_treaty_constraint_boundary, empirical, 'Whether international treaty obligations constitute a binding external limit on the Knesset''s majoritarian authority or are subordinate to legislative reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement_basis(basi_tr_t1992, observed).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(basi_tr_t2000, observed).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement_basis(basi_tr_t2010, observed).
narrative_ontology:measurement(basi_tr_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2018, 0.16).
narrative_ontology:measurement_basis(basi_tr_t2018, observed).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2023, 0.19).
narrative_ontology:measurement_basis(basi_tr_t2023, observed).
narrative_ontology:measurement(basi_tr_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(basi_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.08).
narrative_ontology:measurement_basis(basi_be_t1992, observed).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement_basis(basi_be_t2000, observed).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement_basis(basi_be_t2010, observed).
narrative_ontology:measurement(basi_be_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2018, 0.16).
narrative_ontology:measurement_basis(basi_be_t2018, observed).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2023, 0.17).
narrative_ontology:measurement_basis(basi_be_t2023, observed).
narrative_ontology:measurement(basi_be_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(basi_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.1).
narrative_ontology:measurement_basis(basi_su_t1992, observed).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement_basis(basi_su_t2000, observed).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement_basis(basi_su_t2010, observed).
narrative_ontology:measurement(basi_su_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement_basis(basi_su_t2018, observed).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2023, 0.25).
narrative_ontology:measurement_basis(basi_su_t2023, observed).
narrative_ontology:measurement(basi_su_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(basi_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel basic_law_interpretive_boundary. The kernel itself is the unresolved constitutional question of who holds ultimate interpretive authority over the Basic Laws. The three readings decompose this kernel into three structurally distinct constraints: (1) parliamentary_sovereignty_reading (this file): Knesset has supreme authority, judiciary is advisory; ε ≈ 0.15 (low extraction, because the principle distributes power equally across electoral majorities in rotation). (2) judicial_supremacy_reading: Supreme Court's constitutional review is binding on the Knesset; ε substantially higher (extraction from the legislative majority, protection for minorities and Court's institutional independence). (3) balanced_contestation_reading: both institutions hold legitimate bounded authority; external (international) constraints bind both; ε moderate. Each reading has different beneficiary/victim structures, different stakeholder situations, and different institutional seats. They are linked via network.affects_constraints because a shift in one reading's factual accuracy or political dominance reshapes the others' operating environment. The rising theater_ratio and suppression_requirement in this reading (parliamentary sovereignty) from 2018–2023 reflect the institutional struggle between readings becoming explicit during the 2023–2024 judicial reform debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
