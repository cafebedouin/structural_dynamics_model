% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term as Zone of Legislative Discretion (Rational Basis Deference)
 *   domain: constitutional_law/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the judicial-ambiguity reading of the copyright
 *   constitutional mandate kernel: the claim that copyright term length is a
 *   zone of legislative discretion into which courts decline to inquire
 *   meaningfully, applying rational basis review to preserve
 *   separation-of-powers propriety. This reading is agnostic about whether
 *   the resulting terms are good policy — its distinctive claim is
 *   procedural: the judiciary has converted a textual limitation ('for
 *   limited Times') into a non-justiciable question, which structurally
 *   enables Congress to ratchet terms upward (as in the 1976 and 1998
 *   extensions) without ever facing a binding judicial ceiling. This is
 *   analytically distinct from the corporate_enclosure_reading (which asserts
 *   copyright IS a property right warranting maximal protection as a
 *   substantive matter) and the public_scaffold_reading (which asserts the
 *   public-domain-enrichment purpose should substantively bound term length).
 *   Those are different constraints with different beneficiary structures and
 *   different epsilon values — this reading's epsilon is deliberately
 *   low-to-moderate because the extraction here is a second-order structural
 *   effect (deference enabling drift) rather than a first-order claim about
 *   what copyright should protect.
 *
 * KEY AGENTS:
 *   - congress: primary beneficiary of unconstrained legislative discretion over term length
 *   - copyright_holding_industries: economic beneficiary who lobbies for and profits from each extension enabled by the deferential posture
 *   - federal_judiciary: agenda-setter whose choice of standard of review is the constraint's mechanism
 *   - public_domain_entrants: diffuse payer bearing the cost of delayed public domain entry
 *   - constitutional_fixity_as_a_constraint: non-agent entity whose erosion is the structural victim of this reading
 *   - future_creators_and_remixers: excluded voice with no lobbying presence in the legislative process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term as Zone of Legislative Discretion (Rational Basis Deference)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "constitutional_law/intellectual_property").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '174faec2-7a0f-4ff1-b212-99be3605a127').
narrative_ontology:cs_kernel_codification('174faec2-7a0f-4ff1-b212-99be3605a127', fixed_text).
narrative_ontology:cs_authority_grounding('174faec2-7a0f-4ff1-b212-99be3605a127', lineage).
narrative_ontology:cs_interpretation_layer_present('174faec2-7a0f-4ff1-b212-99be3605a127').
narrative_ontology:cs_reading_relation('174faec2-7a0f-4ff1-b212-99be3605a127', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('174faec2-7a0f-4ff1-b212-99be3605a127', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_axiom('174faec2-7a0f-4ff1-b212-99be3605a127', foundational, term_length_is_non_justiciable_legislative_judgment).
narrative_ontology:cs_axiom_status(term_length_is_non_justiciable_legislative_judgment, holdable).
narrative_ontology:cs_axiom_grounding('174faec2-7a0f-4ff1-b212-99be3605a127', term_length_is_non_justiciable_legislative_judgment, conventional).
narrative_ontology:cs_axiom('174faec2-7a0f-4ff1-b212-99be3605a127', secondary, separation_of_powers_bars_judicial_economic_line_drawing).
narrative_ontology:cs_axiom_status(separation_of_powers_bars_judicial_economic_line_drawing, holdable).
narrative_ontology:cs_axiom_grounding('174faec2-7a0f-4ff1-b212-99be3605a127', separation_of_powers_bars_judicial_economic_line_drawing, instrumental).
narrative_ontology:cs_reference_frame('174faec2-7a0f-4ff1-b212-99be3605a127', eldred_rational_basis_standard).
narrative_ontology:cs_drift_state('174faec2-7a0f-4ff1-b212-99be3605a127', post_1998_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('174faec2-7a0f-4ff1-b212-99be3605a127', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congress).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_entrants).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, separation_of_powers_deference_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term length by ordinary legislation, repeatedly extending it (1831, 1909, 1976, 1998) without ever testing the outer boundary of 'limited Times' against a hard judicial ceiling. Faces sustained lobbying from rights-holding industries and virtually no organized opposition with comparable resources. Each extension is justified by international harmonization or incentive rhetoric rather than an economic showing tied to the constitutional purpose.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Major content owners (film studios, music publishers, estates) receive extended exclusivity windows each time Congress acts, converting works that would otherwise enter the public domain into continued revenue streams. They fund the lobbying that produces each extension and are the direct economic beneficiaries of the judicial posture that never forces Congress to justify the length against the constitutional bargain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries, beneficiary,
    organized, generational, arbitrage, global).

% Reviews challenges to term extensions (as in Eldred v. Ashcroft) but applies rational basis review — the most deferential standard available — treating the 'limited Times' and 'promote progress' clauses as justiciable in theory but functionally unenforceable in practice. The judiciary retains formal authority to invalidate a term as not 'limited' but has never exercised it, effectively converting a textual limit into a procedural formality.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, observer).

% Works that would have entered the public domain under any prior term schedule remain locked under copyright for additional decades with each extension. Libraries, archivists, remix artists, educators, and the general public bear the cost of continued restriction on works whose creators are frequently deceased and whose incentive-to-create rationale has become retroactively inapplicable. They have no standing mechanism to compel a term ceiling; their only recourse is the same deferential courts.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_entrants, payer,
    powerless, civilizational, trapped, national).

% The constitutional text's own claim to impose a real, judicially enforceable boundary ('for limited Times') is the thing eroded by this reading. It is not an actor but a doctrinal commitment; each rational-basis affirmance treats the limitation as satisfied by any term Congress can rationalize, which drains the clause of independent constraining force without ever declaring it void.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint).

% Would benefit from a richer, more current public domain to build upon, but have no organized lobby comparable to rights-holders and no seat at the legislative table when term extensions are negotiated. Their interest in a term ceiling tied to actual incentive economics is structurally unrepresented in the process that sets the term.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators_and_remixers, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rational basis deference lets courts avoid substituting their own economic judgment for Congress's on a genuinely complex, empirically contestable question — how long a copyright term is needed to incentivize creation — preserving the separation-of-powers principle that economic line-drawing belongs to the elected branch.
% TRANSFER_FUNCTION: The judicial posture transfers effective authority over the constitutional boundary from the judiciary (institutional check) to Congress (institutional actor with concentrated lobbying exposure), which in turn transfers value from the public domain (diffuse, unorganized beneficiary of expiring terms) to existing rights-holders (concentrated, organized beneficiary of extended terms).
% ABSENT_VOICES: The public-domain-dependent constituency — libraries, educators, remix culture, future creators — has no lobbying apparatus comparable to rights-holding industries and is not meaningfully present when term-extension bills are drafted or when courts assess whether a given term is still 'limited.' Their absence is structural, not incidental: diffuse future benefit cannot organize as effectively as concentrated present rent.
% DISAPPEARANCE_RATIONALE: If courts abandoned rational basis review for a searching test of what 'limited Times' actually bounds, Congress would face real judicial risk in future extensions, rights-holders would lose a reliable lobbying-to-extension pipeline, and the public domain would gain a credible, enforceable expiration schedule for the first time in over a century. The absence of judicial ambiguity would restructure the entire legislative bargaining dynamic around copyright term.
% FOUNDING_PROBLEM: The judiciary needed a workable standard for reviewing economic and social legislation under the Copyright Clause without turning courts into a super-legislature second-guessing every congressional judgment about incentive structures — rational basis review was imported from general economic due process doctrine to serve this institutional-competence problem.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority in Eldred v. Ashcroft (2003) attests the problem remains live: courts lack the institutional competence to police term length and must defer to Congress's judgment. Dissenting justices (Breyer, Stevens) and outside academic commentary (Lawrence Lessig and others litigating and writing independently of any rights-holder interest) attest that the deference has hardened into a rubber stamp that no longer performs genuine institutional-competence screening, and that 'limited Times' has been rendered non-justiciable in practice — a status the founding rationale never contemplated.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate, not high, because this reading's claim is narrowly procedural: it is the deference mechanism itself, not the substantive length of any given term, that is being measured. The theater ratio (0.58) is elevated because rational basis review performs the function of judicial oversight — hearing the case, articulating a standard, issuing an opinion — while its practical operation forecloses any outcome other than affirmance, making the review largely performative relative to its stated function of policing constitutional limits. Suppression (0.38) reflects that the mechanism forecloses litigation-based challenges to term length without employing direct coercion — the suppression is doctrinal (foreclosing a legal argument) rather than coercive in the ordinary sense. Accessibility collapse (0.45) is moderate: the doctrinal path is closed, but political remedies (electing different legislators, treaty renegotiation) remain nominally open, distinguishing this from a fully collapsed alternative set.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's seat, rational basis deference is simply the correct constitutional posture — courts respecting the political branches' economic judgment, exactly as they do with tax and spending legislation. From the public-domain-entrant seat, the same deference is what permits an textually explicit limitation to be evacuated of independent force. The federal judiciary occupies both seats simultaneously: it authors the deference (agenda_setter) while also being the analytical body that could, in principle, recognize the drift (observer) — this dual position is exactly where the mandatrophy question lives.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and copyright-holding industries sit near the beneficiary end: Congress gains unconstrained policy latitude, and rights-holding industries convert that latitude into concrete term extensions worth billions in continued exclusivity. Public domain entrants and the abstract interest in constitutional fixity sit near the target end: they bear the cost of a limitation clause that no longer binds, with no comparable capacity to organize resistance. The federal judiciary is structurally ambiguous — it is not extracting for itself, but its choice of review standard is the load-bearing mechanism that determines who wins; it is authored here as agenda_setter because the standard-of-review choice is itself the active administrative act being contested, not a neutral background fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding judicial overreach into complex economic line-drawing) may remain partially live as an institutional-competence concern in general, but the specific application to copyright term length is contested: the 'limited Times' clause was drafted as a substantive, judicially cognizable boundary, and rational basis review effectively converts a boundary condition into a standard that is satisfied by any legislative rationalization whatsoever. This is the mismatch the R5 interview is designed to surface — founding_problem_status is authored as contested precisely because Eldred's majority and its dissent disagree about whether the deferential posture still serves the institutional-competence rationale or has become a mechanism for permitting exactly the kind of unbounded extension the clause was written to prevent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deference_as_neutral_vs_captured,
    'Is rational basis review here a neutral, generally-applicable separation-of-powers doctrine correctly applied to a hard institutional-competence problem, or has it been selectively deployed in copyright cases because rights-holding industries have effectively captured the political process that generates the ''rational basis'' Congress must show?',
    'Comparative analysis of rational basis review''s application and outcomes across other constitutional clauses (e.g., Commerce Clause, spending power) versus its application specifically to copyright term challenges — if the standard is applied with unusual laxity only in copyright cases, that supports the captured-deference reading; if it tracks general rational basis jurisprudence, that supports the neutral-doctrine reading.',
    'If neutral, this reading''s extraction is closer to a genuine coordination cost of separation of powers (lower epsilon justified). If captured, the deference itself becomes an extractive mechanism dressed as institutional restraint, and this reading''s epsilon should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_as_neutral_vs_captured, conceptual, 'Whether rational basis deference in copyright is principled institutional restraint or captured doctrine.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and the public_scaffold_reading live — in the interpretation of ''limited Times'' itself, or in the standard of review courts should apply to enforce whatever ''limited Times'' means?',
    'Doctrinal analysis distinguishing substantive constitutional meaning (what counts as ''limited'') from institutional enforcement mechanism (what standard of review polices that meaning) — these are logically separable questions that the kernel''s natural-language framing conflates.',
    'If the disagreement is purely about the standard of review (this reading''s position) rather than substantive meaning, then this reading and public_scaffold_reading could in principle both be true simultaneously at different institutional layers, which would argue for influences rather than coexists_with as the relation. If the disagreement is actually about substantive meaning smuggled into procedural language, the readings are more sharply opposed than currently modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating whether the reading split is about interpretation or about enforcement standard.').

omega_variable(
    eldred_as_settled_or_contestable,
    'Does Eldred v. Ashcroft (2003) permanently settle the judicial-ambiguity reading as constitutional doctrine, or does it remain contestable given the closeness of the vote, the force of the Breyer/Stevens dissents, and subsequent scholarly criticism?',
    'Track subsequent circuit court treatment of Eldred, any certiorari grants revisiting term-length challenges, and legislative scholarship citing Eldred as either settled or ripe for reconsideration.',
    'If settled, the judicial_ambiguity_reading''s structural stability is high and its founding_problem_status should trend toward ''live'' as the operative doctrine. If genuinely contestable, the reading''s persistence depends more on inertia than on active doctrinal consensus, which would support a ''dead'' or more strongly ''contested'' founding_problem_status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eldred_as_settled_or_contestable, empirical, 'Whether the Eldred precedent forecloses future substantive challenges to copyright term length.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(copy_tr_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1986, 0.35).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.5).
narrative_ontology:measurement(copy_tr_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2003, 0.55).
narrative_ontology:measurement(copy_tr_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2012, 0.58).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.22).
narrative_ontology:measurement(copy_be_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1986, 0.28).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement(copy_be_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(copy_be_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.2).
narrative_ontology:measurement(copy_su_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1986, 0.24).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.34).
narrative_ontology:measurement(copy_su_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement(copy_su_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.1).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the copyright_constitutional_mandate kernel. corporate_enclosure_reading treats copyright as a property right warranting maximal protection (highest epsilon, most extractive of the three, victim: public domain generally). public_scaffold_reading treats the public-domain-enrichment purpose as substantively binding on term length (lowest epsilon, most rope-like, victim: rights-holders denied indefinite extension). This judicial_ambiguity_reading occupies the procedural middle: it authors no substantive position on how long terms should be, but its structural effect is to REMOVE the judicial mechanism that would otherwise force a choice between the other two readings, functionally enabling drift toward the corporate_enclosure_reading's practical outcome over time. It is upstream of both siblings in the causal sense: the standard of review determines how much room the substantive disagreement between enclosure and scaffold readings has to play out in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__judicial_ambiguity_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
