% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional/political/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the judicial supremacy reading of the
 *   constitutional authority boundary kernel: the constitutional text is read
 *   as establishing courts as final, unchallengeable arbiters of
 *   constitutional meaning, with exclusive power to invalidate legislative
 *   and executive action. This is ONE reading of a contested kernel; sibling
 *   readings (coordinate construction, parliamentary primacy) are OTHER
 *   constraints, not parts of this one. The story describes how judicial
 *   supremacy operates: who benefits (judiciary, constitutional minorities),
 *   who bears costs (elected legislature, executive), and what structural
 *   features sustain it (finality doctrine, high amendment barriers, weak
 *   accountability). The constraint exhibits tangled-rope structure: it
 *   solves a genuine coordination problem (constitutional constraint on
 *   majorities) while simultaneously extracting from the legislature through
 *   interpretive monopoly. The rising extractiveness trajectory (0.42 → 0.68)
 *   models the accumulation of judicial veto power over decades as
 *   constitutional doctrine expands; the plateau after year 30 reflects
 *   equilibration where the theater-ratio also stabilizes.
 *
 * KEY AGENTS:
 *   - Judiciary: institutional agenda-setter, holds interpretive monopoly, collects rents through discretion
 *   - Elected legislature: institutional payer, bears veto costs and constrained policy space
 *   - Executive branch: dual-positioned (payer from judicial constraint, beneficiary from judicial limits on legislative oversight)
 *   - Organized public: dual-positioned (beneficiary from rights protection, payer through foreclosed majorities)
 *   - Constitutional amendment mechanism: non-agent observer, the theoretical but rarely used remedy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional/political/institutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '6d661b03-6688-4061-9b1d-147c94861e1d').
narrative_ontology:cs_kernel_codification('6d661b03-6688-4061-9b1d-147c94861e1d', fixed_text).
narrative_ontology:cs_authority_grounding('6d661b03-6688-4061-9b1d-147c94861e1d', lineage).
narrative_ontology:cs_interpretation_layer_present('6d661b03-6688-4061-9b1d-147c94861e1d').
narrative_ontology:cs_reading_relation('6d661b03-6688-4061-9b1d-147c94861e1d', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('6d661b03-6688-4061-9b1d-147c94861e1d', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('6d661b03-6688-4061-9b1d-147c94861e1d', foundational, constitutional_review_monopoly).
narrative_ontology:cs_axiom_status(constitutional_review_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('6d661b03-6688-4061-9b1d-147c94861e1d', constitutional_review_monopoly, deontological).
narrative_ontology:cs_axiom('6d661b03-6688-4061-9b1d-147c94861e1d', secondary, finality_doctrine_necessity).
narrative_ontology:cs_axiom_status(finality_doctrine_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6d661b03-6688-4061-9b1d-147c94861e1d', finality_doctrine_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('6d661b03-6688-4061-9b1d-147c94861e1d', judicial_constitutional_supremacy).
narrative_ontology:cs_drift_state('6d661b03-6688-4061-9b1d-147c94861e1d', contemporary_legislative_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d661b03-6688-4061-9b1d-147c94861e1d', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, organized_public).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, counter_majoritarian_constituencies).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, organized_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses exclusive interpretive authority over constitutional meaning and the power to invalidate legislative and executive acts deemed unconstitutional. Justifies this authority as necessary to preserve the supremacy of constitutional law above legislative will. Collects monopoly rents through interpretive discretion—the ability to define the boundary between constitutional and unconstitutional without institutional check or legislative override mechanism.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacts legislation within bounds defined retroactively by judicial decision. Bears the cost of judicial veto: invalidated statutes, constrained policy space, and the burden of constitutional amendment (requiring supermajority coordination) as the only remedy for judicial constitutional readings the legislature opposes. Cannot override or revise judicial constitutional interpretation through ordinary legislative process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature, payer,
    institutional, biographical, constrained, national).

% Bound by judicial constitutional limits on executive authority. Experiences dual pressure: judiciary constrains executive action through constitutional doctrine, yet judiciary may also limit legislative checks on executive power (strengthening executive vis-à-vis legislature). The executive pays through narrowed action space but may benefit selectively when courts limit legislative constraints.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, beneficiary).

% Receives protection against legislative majorities that would violate fundamental rights (the coordination benefit: courts enforce constitutional constraints). Also bears costs where judicial constitutional readings foreclose popular legislative majorities and require constitutional amendment to revise—diffuse cost, concentrated benefit to rights-holding minorities.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, organized_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, organized_public, payer).

% Other branches' constitutional interpretations (legislative or executive readings of their own authority) are structurally subordinate to judicial review. Excluded from coequal interpretive standing; their constitutional claims require judicial validation to carry legal force. Constrained by finality doctrine that treats judicial readings as binding while treating coequal-branch readings as provisional or merely persuasive.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, competing_interpretive_authority, excluded,
    institutional, generational, trapped, national).

% Benefit when courts enforce constitutional rights that legislatures would otherwise suppress (minority religious practice, political dissent, due process). Depend on judicial supremacy to protect against majoritarian legislative preference. Exit requires constitutional amendment or replacement of the judiciary, both extremely costly and rare.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, counter_majoritarian_constituencies, beneficiary,
    powerless, generational, constrained, national).

% The formal escape hatch from judicial constitutional readings: supermajority legislative action can amend the Constitution and override judicial interpretation. Rarity and supermajority requirement structure this as a theoretical remedy rather than a practical override mechanism, making judicial finality nearly absolute.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_amendment_mechanism, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_amendment_mechanism).

% Judiciary operates with reduced public accountability relative to elected branches. No electoral check, high removal barriers, lifetime tenure. The weak accountability mechanism means the constraint persists regardless of public opposition to specific judicial constitutional readings.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, public_accountability_mechanism, observer,
    analytical, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_authority_boundary__judicial_supremacy_reading, public_accountability_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule of law above legislative will: constitutional law is treated as binding on all branches, enforced by courts with power to invalidate legislative and executive action. Solves the problem of constitutional constraint—ensuring that majorities cannot revise fundamental law through ordinary legislation.
% TRANSFER_FUNCTION: Moves interpretive authority from distributed (all branches interpreting the constitution within their domain) to concentrated (judiciary alone authoritatively interprets). Transfers policy veto power from democratic majorities to appointed judges, restricting legislative and executive discretion within bounds set by judicial doctrine.
% ABSENT_VOICES: Coordinate-construction and parliamentary-primacy readings are structurally excluded from the authoritative conversation; their interpretations of the Constitution are treated as subordinate, provisional, or merely persuasive pending judicial validation. Legislative and executive branch constitutional arguments are heard only as claims subject to judicial review, not as coequal interpretations. Popular majorities opposing a judicial constitutional reading have no avenue to override it except through the supermajority amendment process.
% DISAPPEARANCE_RATIONALE: If judicial constitutional supremacy vanished (replaced by coequal interpretation or legislative override authority), the boundary of constitutional meaning would shift dramatically: legislative majorities would define constitutional limits on their own power, executive action would be constrained by legislative rather than judicial review, and fundamental rights would depend on legislative rather than judicial protection. The entire structure of constitutional constraint reorganizes around a different authority center.
% FOUNDING_PROBLEM: Early constitutional democracy lacked a mechanism to enforce constitutional limits on legislative majorities. Legislatures could expand their own authority through ordinary law, reading away constitutional constraints, and minorities had no remedy. The founding problem is the risk of constitutional erosion through legislative amendment or majoritarian disregard.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and constitutional scholars defending judicial review attest the problem remains live: legislatures have incentives to expand their own power and majorities threaten constitutional minorities. Proponents of legislative supremacy and coequal interpretation attest the problem is oversold: legislatures have proved capable of self-restraint, constitutional text is reasonably durable, and judicial veto power creates a different problem (counter-majoritarian constraint on legitimate legislative action). Comparative constitutional history (UK parliamentary supremacy, Canadian coequal-branch interpretation, EU supranational review) documents different solutions. The founding problem's status is genuine disagreement on whether legislative self-restraint is reliable enough, making the status contested rather than resolved.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval, reflecting how judicial supremacy accumulates veto power through doctrine expansion (substantive due process, equal protection, dormant commerce clause). Early extractiveness is lower because the doctrine is narrower and legislatures retain more policy discretion; by year 30 the doctrine has expanded to cover more policy domains, tightening the constraint on elected branches. Theater ratio is consistently low (0.12-0.28) because the core coordination function—enforcing constitutional limits—is genuinely performed; the rising ratio reflects increasing use of constitutional doctrine for non-foundational policy preferences (expansion of judicial review scope beyond clear constitutional violation), but the ratio remains moderate because the foundational function persists. Suppression is high (0.58-0.72) because persistence depends on finality doctrine and removal barriers that prevent legislative override; the legislature cannot escape the constraint through ordinary politics. The plateau after year 30 indicates equilibrium: extractiveness and suppression stabilize once constitutional doctrine reaches coverage saturation and judicial institutional strength becomes durable. This is NOT a piton (the coordination function is live and the enforcement machinery works); it is a tangled rope that has matured and stabilized.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and constitutional scholars defending judicial review would classify this as pure rope: a genuine coordination mechanism (enforcing constitutional limits) with real beneficiaries (constitutional minorities) and manageable costs (elected branches retain policy discretion in unconstitutional matters). Elected legislatures and democracy advocates would classify it closer to snare: interpretive monopoly that forecloses legislative override, veto power decoupled from democratic authorization, and suppression maintained through removal barriers that prevent electoral correction. The engine should compute different types at different seats because the structural asymmetry is real: from the judiciary's position the arrangement solves a genuine problem it was built for; from the legislature's position it is enforced extraction with no viable exit except constitutional amendment. This divergence is the measurement the corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary: beneficiary with institutional power and arbitrage-grade exit (can maintain the constraint through finality doctrine regardless of external pressure, or can relinquish it through self-restraint—strategically mobile). Directionality near 0.0 (beneficiary end): collects monopoly rents from interpretation, sets the agenda, controls the boundary. Elected legislature: victim with institutional power but constrained exit (can only override through constitutional amendment, nearly impossible). Directionality near 1.0 (target end): bears veto costs, has policy space restricted, cannot translate electoral mandate into constitutional interpretation. Executive branch: mixed. When judiciary constrains executive power, directionality is high (target end); when judiciary limits legislative checks on executive, directionality shifts toward beneficiary end. Average d for executive should be slightly above 0.5 (mild target bias, because more of the doctrinal constraint falls on executive administrative action than on executive constitutional interpretation). Organized public: beneficiary when courts protect minority rights (low d), payer when courts foreclose popular majorities (high d), averaging near 0.5. The directionality derivation chain (beneficiary/victim + exit options + power) should produce these seat-differentiated d values automatically; no overrides needed unless a specific institutional actor's exit options differ from power level in ways the schema's atoms don't capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional erosion through legislative amendment) was live in the 18th century. By the 20th century it was substantially dead: legislatures proved capable of self-restraint, constitutional text is durable, and comparative democracies solved the problem differently. The constraint persists despite dead founding problem because: (1) it solves a secondary problem now (rights protection against majorities) that keeps it functionally valuable; (2) the suppression mechanism (finality doctrine + removal barriers) makes it durable regardless of founding-problem status; (3) the judiciary benefits from the arrangement and has institutional power to maintain it. This is NOT mandatrophy yet (the constraint still serves a real function: rights protection). Mandatrophy would arrive if the secondary function also atrophied—if courts became purely theatrical defenders of rights while legislatures built de facto override mechanisms, or if the amendment process became effectively accessible, making finality doctrine unenforceable. The constraint exhibits early-stage mandatrophy risk: theater ratio is rising and the founding problem is contested, indicating the primary rationale is weakening. The rising extractiveness concurrent with moderate theater ratio suggests the constraint is capturing additional rents (policy veto) beyond the founding coordination function. A true mandatrophy verdict would require theater ratio to breach 0.6+ while resistance to override mechanisms remains high—that combination signals pure persistence through institutional inertia rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_authority_epistemic_grounding,
    'Is judicial authority to interpret the constitution grounded in superior institutional epistemic access (courts are better positioned to discern constitutional meaning) or in superior institutional incentives (courts are better aligned to enforce constitutional limits)?',
    'Comparative analysis of judicial vs. legislative constitutional reasoning quality, budget constraints, expertise distribution, and historical track record of institutional error-correction. Examine whether legislatures have misread the constitution more often than courts, or whether apparent disagreement reflects different normative weightings rather than epistemic failure.',
    'If grounded in epistemic access, the constraint''s justification is robust to changes in institutional incentives; if grounded in incentive alignment, changes in court composition or removal mechanisms could undermine the justification and expose the constraint as pure extraction. This distinction affects whether the constraint should be classified as tangled rope (genuine coordination + asymmetric extraction) or snare (extraction using coordination framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_epistemic_grounding, empirical, 'Whether judicial interpretive authority rests on epistemic or incentive grounds.').

omega_variable(
    finality_doctrine_resilience,
    'Is the finality doctrine (the rule that judicial constitutional interpretation is binding and not subject to legislative override) structural to the constraint or an institutional convention that could be revised?',
    'Historical analysis of jurisdictions that have adopted or rejected finality doctrine, and institutional experiments with override mechanisms (referenda, legislative supermajority revision, constitutional courts with sunset clauses). Document whether finality doctrine is enforced by constitutional text or by institutional practice and judicial self-enforcement.',
    'If finality doctrine is embedded in constitutional text, the constraint is nearly irreversible and suppression remains high indefinitely. If finality doctrine is institutional convention, it could be revised through ordinary institutional evolution, converting the constraint into a negotiable-equilibrium situation rather than a fixed supremacy rule. This affects whether the constraint classifies as durable rope or fragile piton masquerading as rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finality_doctrine_resilience, empirical, 'Whether finality doctrine is constitutional text or institutional convention.').

omega_variable(
    rights_protection_vs_policy_veto_divergence,
    'How much of the measured extractiveness (0.68) represents genuine rights protection (the coordination function) versus policy veto power unrelated to constitutional constraint?',
    'Categorize judicial invalidations by domain: constitutional rights protection (First Amendment, equal protection, due process narrowly construed), structural constitutional limits (separation of powers, federalism narrowly construed), and policy-preference veto (substantive due process expansions, dormant commerce clause, unenumerated rights doctrine). Model extraction as weighted by category: pure rights protection counts fully toward coordination cost, policy veto counts fully toward extraction rent.',
    'A high ratio of policy veto to genuine rights protection would indicate the constraint is increasingly a snare: using constitutional supremacy framing to extract policy veto power beyond the founding problem''s scope. A low ratio would support the tangled-rope classification: genuine coordination (rights protection) with asymmetric costs (legislative veto) but real benefits distributed. This determination is critical for mandatrophy assessment: if policy veto is rising faster than rights protection, the constraint is accumulating rent beyond its founding function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_vs_policy_veto_divergence, empirical, 'The proportion of extractiveness attributable to rights protection versus policy veto.').

omega_variable(
    comparative_reading_legitimacy,
    'Under what conditions would the coordinate-construction or parliamentary-primacy readings become the dominant reading of the same constitutional text?',
    'Institutional change scenarios: (1) judicial legitimacy collapse (court composition polarization, mass public distrust reducing enforcement of finality doctrine); (2) legislative capacity improvement (legislatures develop better constitutional reasoning infrastructure); (3) amendment mechanism accessibility (if amendment becomes easier, legislative override becomes viable); (4) comparative institutional performance (empirical demonstration that coequal or parliamentary authority produces better constitutional outcomes). Document which condition(s) would shift interpretive authority away from judicial supremacy.',
    'This omega documents the fragility of the judicial supremacy reading''s hold on the kernel. If legitimacy collapse or legislative capacity improvement is plausible in foreseeable time horizons, the reading is contingent rather than stable. This informs the constraint''s terminal-state trajectory: is judicial supremacy likely to persist indefinitely, or is it in competition with alternative readings for the same kernel''s authority structure?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(comparative_reading_legitimacy, conceptual, 'Under what institutional conditions would sibling readings displace judicial supremacy as the dominant interpretation.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) externally enforced (through finality doctrine and removal barriers that the legislature cannot escape) or internalized (through judicial legitimacy and public acceptance of finality doctrine, such that the legislature would not override even if it could)?',
    'Test through institutional experiments: jurisdictions that weaken finality doctrine or judicial removal barriers while maintaining strong public commitment to judicial review. If suppression persists despite institutional weakening, internalization is high. If suppression decays when institutional enforcement weakens, suppression is structural/external.',
    'If suppression is highly internalized, the constraint could persist even if institutional enforcement weakens, because legislatures and publics accept finality doctrine as legitimate. If suppression is structural, weakening enforcement mechanisms would quickly erode the constraint. This affects both type classification (higher internalization supports rope reading; higher structurality supports snare reading) and long-term trajectory prediction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of legislative override is external institutional fact or internalized legitimacy acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t5, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t15, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cons_tr_t35, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t5, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(cons_be_t15, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t35, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cons_su_t5, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(cons_su_t15, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(cons_su_t25, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(cons_su_t35, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__judicial_supremacy_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, legislative_override_mechanisms).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_amendment_mechanics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested constitutional authority boundary kernel. All three readings share the same referent (the constitutional text's authority structure) but interpret it fundamentally differently, producing different constraint types with different beneficiary/victim structures and ε values. The judicial supremacy reading (this story) asserts exclusive judicial authority (ε=0.68, tangled rope). The coordinate-construction reading asserts coequal branch authority (lower ε, pure rope or mountain depending on coordination success). The parliamentary-primacy reading asserts legislative supremacy (ε lower or higher depending on legislative vs. judicial capture, likely tangled rope or snare from different seats). All three stories must be authored separately per the ε-invariance principle; this network link documents the family relationship and the fact that the readings are in direct structural competition for the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
