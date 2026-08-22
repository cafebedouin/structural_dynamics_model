% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary-supremacy reading of the
 *   constitutional-interpretive-authority kernel. Under this reading, the
 *   elected legislature possesses final authority to interpret the
 *   constitution; courts possess no power to void parliamentary acts on
 *   constitutional grounds. The legislature is the beneficiary of
 *   unconstrained interpretive discretion. Constitutional minorities and
 *   rights claimants lacking a legislative majority are the victims — they
 *   cannot invoke judicially-enforceable constitutional limits on legislative
 *   interpretation. This reading competes with two sibling readings of the
 *   same kernel: the judicial-supremacy reading (courts hold final authority
 *   via rights guardianship) and the coordinate-construction reading (no
 *   single branch is final; the constitution is constructed through
 *   inter-branch dialogue). The claim/metric gap is deliberate and
 *   structurally important: the reading CLAIMS tangled_rope (genuine
 *   coordination problem of multiple institutional sites claiming authority,
 *   solved via hierarchical assignment to legislature; some beneficiary
 *   coordination benefit exists), while the authored metrics describe
 *   substantially extractive operation (0.68 extractiveness, 0.71
 *   suppression) with rising theater (0.42). The engine will measure whether
 *   this classification holds or whether the metrics better describe snare or
 *   piton. Do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - elected_legislature: institutional beneficiary, agenda-setter; possesses final interpretive authority and collects the benefit of unconstrained discretion
 *   - constitutional_minorities: powerless victims, trapped; cannot command legislative majorities and lack judicial recourse against majoritarian interpretation
 *   - rights_claimants_lacking_legislative_majority: moderate-power victims; seek constitutional protection that legislative interpretation can defeat
 *   - judicial_branch: excluded institutional actor, identity-locked; their professional mandate (guard constitutional boundaries) conflicts with accepting legislative finality
 *   - rival_constitutional_democracies: analytical observers; provide comparative evidence on outcomes of parliamentary supremacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '7714c83c-72ea-4582-a681-fbbfc0e7a339').
narrative_ontology:cs_kernel_codification('7714c83c-72ea-4582-a681-fbbfc0e7a339', formalized).
narrative_ontology:cs_authority_grounding('7714c83c-72ea-4582-a681-fbbfc0e7a339', lineage).
narrative_ontology:cs_interpretation_layer_present('7714c83c-72ea-4582-a681-fbbfc0e7a339').
narrative_ontology:cs_reading_relation('7714c83c-72ea-4582-a681-fbbfc0e7a339', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('7714c83c-72ea-4582-a681-fbbfc0e7a339', constitutional_interpretive_authority__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('7714c83c-72ea-4582-a681-fbbfc0e7a339', foundational, electoral_mandate_is_supreme_legitimacy).
narrative_ontology:cs_axiom_status(electoral_mandate_is_supreme_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7714c83c-72ea-4582-a681-fbbfc0e7a339', electoral_mandate_is_supreme_legitimacy, conventional).
narrative_ontology:cs_axiom('7714c83c-72ea-4582-a681-fbbfc0e7a339', foundational, constitutional_meaning_determined_by_legislature).
narrative_ontology:cs_axiom_status(constitutional_meaning_determined_by_legislature, holdable).
narrative_ontology:cs_axiom_grounding('7714c83c-72ea-4582-a681-fbbfc0e7a339', constitutional_meaning_determined_by_legislature, deontological).
narrative_ontology:cs_reference_frame('7714c83c-72ea-4582-a681-fbbfc0e7a339', parliamentary_sovereign_will).
narrative_ontology:cs_drift_state('7714c83c-72ea-4582-a681-fbbfc0e7a339', contemporary_rights_inflation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7714c83c-72ea-4582-a681-fbbfc0e7a339', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants_lacking_legislative_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_amendment_authority).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, democratic_mandate_legitimacy).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_accountability_primary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses final interpretive authority over the constitution; enacts legislation without fear of judicial nullification for constitutional grounds. Claims legitimacy through electoral mandate and direct democratic representation. Administers the constraint by resisting and invalidating judicial attempts to strike down legislation. Collects the benefit of unconstrained legislative discretion — can interpret the constitutional text to justify policy preferences without external constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Groups that cannot command a legislative majority and seek protection from majoritarian policies. Under this reading, they lack recourse when the legislature interprets the constitution to authorize policies that harm them. Their only remedies are political persuasion (building a new majority) or exit — both high-cost or impossible. They bear the cost of interpretive discretion they cannot constrain.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities, payer,
    powerless, generational, trapped, national).

% Individuals and organized groups claiming constitutional rights that conflict with legislative policy. They pay the cost of a reading that treats legislative interpretation as final — their rights claims are defeated if the legislature reads the constitution to authorize the challenged action. They can lobby for legislative reconsideration, seek constitutional amendment (extremely high-cost), or migrate if the jurisdiction permits exit.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants_lacking_legislative_majority, payer,
    moderate, biographical, constrained, national).

% Is excluded from final interpretive authority under this reading. Courts would argue that constitutional rights require external enforcement against majoritarian legislatures, and that courts exist precisely to check legislative interpretive overreach. The exclusion is structural: their professional identity and institutional mandate (protect constitutional boundaries) are incompatible with accepting legislative finality, yet they are bound by the constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch, excluded,
    institutional, generational, identity_locked, national).

% Constitutional systems (UK, Germany, Canada) operate under variants of this reading or coordinate-construction readings. They observe the constraint and the outcomes it produces, providing comparative evidence about whether parliamentary supremacy protects or erodes constitutional legitimacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rival_constitutional_democracies, observer,
    analytical, generational, analytical, global).

% The formal amendment power (typically super-majoritarian legislatures or referenda) benefits from the constraint: if courts could void legislation on constitutional grounds, the amendment path would become less salient as a way to revise the constitutional text. Parliamentary supremacy channels disagreement through amendment politics rather than judicial reinterpretation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_amendment_authority, beneficiary,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of multiple institutional sites (legislature, courts, executives) claiming authority to interpret the constitution. By assigning final authority to the elected legislature, it creates a clear, single canonical voice for constitutional meaning. Participants know that whatever interpretation the legislature endorses is the one that governs; this removes uncertainty and prevents inter-institutional conflict from paralyzing governance.
% TRANSFER_FUNCTION: Transfers interpretive discretion and policy-authority from a potentially constrained institutional position (subject to judicial review) to an unconstrained one (the legislature as final arbiter). The legislature gains freedom to interpret the constitution broadly to justify policies it enacts; minorities and rights claimants lose the ability to invoke judicially-enforceable constitutional limits that might overrule legislative interpretation.
% ABSENT_VOICES: Excluded are the judiciary (structurally barred from claiming final authority and prevented by the constraint from reasserting it) and constitutional minorities and rights claimants who cannot persuade a legislative majority (they would argue for external constraint on majoritarian interpretation but lack the political power to secure legislative agreement). Also absent are supranational human-rights bodies and international constitutional courts that in some democracies can review domestic constitutional interpretation — the reading excludes them entirely from the conversation.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy as a constitutional reading dissolved — if courts claimed final authority or if coordinate-construction became the organizing principle — the institutional structure of constitutional governance would reorganize completely. Judicial review would become the mechanism for resolving constitutional disputes; amendment and reinterpretation would compete as paths to constitutional change; minorities would gain access to courts as a forum to challenge legislative interpretation. The flow of constitutional authority and the distribution of institutional power would shift fundamentally.
% FOUNDING_PROBLEM: The constitutional coordination problem: multiple institutional sites (legislature, courts) naturally claim authority to interpret the constitution, producing conflict and uncertainty about which interpretation governs. An early solution was to assign final authority to the legislative branch as the most directly elected and accountable institution, preventing courts from overriding the people's representatives.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary supremacy advocates (UK constitutional scholars, Westminster-system legislators) attest the founding problem is ongoing and solved by legislative finality — courts that claim review authority create the problem, not solve it. Judicial-supremacy scholars and observers from constitutional-court democracies (Germany, Canada, South Africa) attest that the founding problem is different: the need to protect constitutional rights and democratic foundations against majoritarian legislatures — and that parliamentary supremacy does not solve this problem but abandons it. International human-rights bodies and post-colonial constitutional scholars document harms from unconstrained legislative interpretation (discriminatory legislation unchecked, minority rights eroded), corroborating that the problem remains and parliamentary supremacy leaves it unresolved.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness scores rising from 0.55 to 0.68 over the interval reflect increasing use of interpretive discretion to authorize policies that harm minorities and rights claimants without their consent — the legislature interprets the constitution expansively to justify coercive action. Suppression (0.71) is high because maintaining the constraint requires actively preventing judicial attempts to reassert review authority; every challenge by courts or rights advocates must be rejected or overridden by legislative action reasserting supremacy. Theater ratio (0.42, moderate) reflects that constitutional justifications offered by the legislature are part genuine legal reasoning (coordination problem of multiple authority sources is real) and part cover story for extractive discretion — the constraint is presented as democracy-protective when it often operates as majority-protective. The measurement trajectory shows the constraint stabilizing: extractiveness and suppression rise steeply for the first 15 time points (0-15), then level off (15-40), suggesting the reading has reached a stable equilibrium where legislative majorities have accepted their interpretive power and resistance has plateaued (either minoritized or depoliticized). The shared time grid anchors all three metrics at every measured point; no metric is missing from any time slice.
 *
 * PERSPECTIVAL GAP:
 *   The legislature (beneficiary seat) and the victim seats should compute substantially different types from the same structural data. From the legislature's institutional position, this reading coordinates competing authority claims and gives electoral representatives (the most directly accountable institution) final say — they experience a rope: genuine coordination function, symmetric cost-benefit (legislatures must respect electoral accountability constraints). From the rights-claimant seat, the same structure operates as enforced extraction: no external constraint on majoritarian interpretation, identity-locked judges cannot defend rights, no appeal beyond the legislature — they experience snare or tangled-rope at least. From the judiciary's excluded seat, it is simultaneously a tangled rope (they are coordinated into a subordinate position, genuine problem solved) and a source of extractive suppression (their professional mandate is structurally incompatible with the role assigned). The engine computes these divergences from the per-seat directionality derivation (beneficiary d near 0, victims d near 1, excluded d ambiguous — toward 1 by structural suppression, toward 0 by coordination benefit from unified authority). Do not attempt to reconcile these computed divergences — they are the signal the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature benefits from unconstrained interpretive discretion without bearing the cost of defending minorities' rights claims — directionality for the legislature is low (d near 0.1-0.2), approaching full beneficiary. Constitutional minorities pay the cost (higher majoritarian policies, no judicial escape) with no benefit from the legislative interpretation they are subject to — directionality near 1.0 (full target). Rights claimants sit between (some benefit from general rule of law coordinated by legislature, substantial cost when interpretation defeats their claims) — directionality around 0.65-0.75. The judiciary is trapped in an excluded position: structurally suppressed (cannot claim authority) but coordinationally benefited (unified interpretation, no inter-branch conflict disrupting their work) — directionality ambiguous, likely 0.4-0.6. Overrides are not needed here; the derivation from beneficiary/victim declarations and exit options (trapped minorities, identity-locked judges) produces defensible d values naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of this reading is real: multiple institutional sites (legislature, courts) do claim interpretive authority, and some solution is necessary. Parliamentary supremacy solves this by hierarchy: the legislature interprets finally. The coordination function is genuine. However, a secondary mandate — protecting constitutional rights against majoritarian overreach — is substantially atrophied in this reading. Rights protection becomes contingent on legislative majority support rather than on constitutionalized limits. The reading does not claim to provide rights protection (judicial-supremacy does); it trades rights protection for coordination. The theater ratio of 0.42 reflects that constitutional justifications offered by legislatures for their interpretive moves often invoke rights language and constitutional values even as the reading structurally forecloses judicial enforcement of those values. The mandatrophy is partial: the coordination mandate is live, the rights-protection mandate is absent-by-design. This is not a mandatrophy case in the strict sense (not a constraint whose primary function has atrophied while the constraint persists); it is a reading that deliberately subordinates one mandate to another. No mandatrophy_resolved flag is triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is parliamentary supremacy a distinct constitutional reading instantiating one pole of a genuine triadic kernel-level contest, or is it empirically coincident with one trajectory among contestable mechanisms (electoral cycles, institutional drift) such that the ''reading'' label overstates its independence?',
    'Genealogical analysis: trace the reading''s emergence in legal and political thought; audit whether sibling readings (judicial-supremacy, coordinate-construction) are genuinely orthogonal to it or parasitic framings of the same institutional dynamic. Determine whether the three readings have different foundational axioms or whether they are re-descriptions of the same legislature-court equilibrium under different names.',
    'If the readings are empirically distinct commitments with independent axioms and founding-problem framings, the kernel reading treatment is structurally sound. If they are re-descriptions of a single equilibrium, the triadic framing is conceptually over-determined and the constraint should be reframed as an institutional-competition story with multiple vocabularies rather than a true kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether parliamentary supremacy is a kernel reading or an institutional-equilibrium re-description.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of judicial authority (and by extension, rights claims against the legislature) a structural coercive fact — the judiciary is forced to accept parliamentary finality by legal rules and enforcement — or an internalized submission where judges accept supremacy as a matter of professional identity and constitutional theory?',
    'Institutional history: examine whether judicial acceptance of parliamentary supremacy persists when the legal rules enforcing it are relaxed or challenged. If courts attempt to reassert review authority whenever formal constraints loosen, suppression is partly structural; if courts maintain deference even when rules are ambiguous, suppression is internalized. Survey judicial and legal-academic opinion on whether parliamentarianism is ''correct'' versus ''imposed.''',
    'If suppression is structural, external constraint (legal reform, constitutional amendment) can break it. If internalized, the judges themselves would need to revise their professional ideology and understanding of constitutional limits on their role — a much harder transformation. The effective persistence of the constraint differs by mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether judicial deference to parliamentary supremacy is structurally coerced or professionally internalized.').

omega_variable(
    electoral_mandate_legitimacy_stability,
    'Does the electoral mandate legitimacy that this reading invokes (the legislature''s claim to final authority resting on democratic election) remain stable when elections become non-competitive, gerrymandered, or captured by special interests? At what point does electoral legitimacy erode enough to undermine the reading''s justification?',
    'Empirical tracking of electoral competitiveness, gerrymandering indices, and campaign-finance concentration in jurisdictions operating under parliamentary-supremacy readings. Correlate declining electoral legitimacy with rising resistance to parliamentary finality and increased salience of alternative readings (judicial check, coordinate construction). Document threshold effects: does the reading collapse when some legitimacy threshold is crossed, or does it persist theatrically even without genuine electoral mandate?',
    'If electoral legitimacy is the axiom grounding parliamentary supremacy, loss of competitive elections undermines the reading''s foundational justification. The constraint would persist via inertia and institutional habit rather than genuine legitimacy — risking transformation into a snare (extraction without coordination function) or piton (performance masking lost function). A high theater ratio with declining electoral legitimacy would signal this degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_mandate_legitimacy_stability, empirical, 'Whether parliamentary supremacy''s legitimacy is robust to electoral degradation.').

omega_variable(
    rights_versus_coordination_incommensurability,
    'Is the tension between this reading and judicial-supremacy a matter of competing institutional designs both solving the same coordination problem differently (what institutional forum should resolve constitutional disputes?), or is it a deeper clash between incommensurable frameworks: coordination-via-electoral-mandate versus rights-protection-via-external-constraint?',
    'Comparative institutional analysis: examine whether a hybrid reading (coordinate construction) successfully mediates the two, or whether hybrid attempts collapse back to one pole (courts or legislature taking de facto finality). Analyze whether rights claimants and majoritarians can coherently hold both values simultaneously within one framework, or whether they entail contradictory commitments about what the constitution is for.',
    'If the readings represent different answers to THE SAME question (how to allocate interpretive authority), they coexist contingently — one can influence or foreclose the other. If they represent answers to DIFFERENT questions (how to govern vs. how to protect rights), they may be incommensurable — no single constitutional framework can fully hold both. The reading-relations logic and the possibility of compromise differ by this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_versus_coordination_incommensurability, conceptual, 'Whether the kernel contest is over institutional design or foundational constitutional value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the triadic kernel 'constitutional_interpretive_authority.' The kernel is structured around the question of final interpretive authority in constitutional governance. This reading (parliamentary supremacy) asserts the elected legislature holds final authority. The sibling readings — judicial supremacy and coordinate construction — are distinct constraints with different ε values, different beneficiary/victim structures, and different institutional equilibria. All three readings share the kernel (the persistent constitutional question) but instantiate different answers. Links are directional: parliamentary supremacy influences both siblings by claiming definitional authority; judicial supremacy and coordinate construction influence parliamentary supremacy by contesting its legitimacy and structural stability. Decomposition is warranted by ε-invariance: a measurement of 'interpretive authority in constitutional governance' yields different ε depending on which reading frames the referent (the reading's endorsed answer is never the referent — the referent is the standing arrangement being contested, assessed by each reading's lights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
