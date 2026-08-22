% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the
 *   constitutional-text kernel: courts are the final and conclusive
 *   interpreters of constitutional meaning, and legislative invalidation
 *   carries no override. This reading treats the coordination function
 *   (protecting rights-claimants from majoritarian overreach, providing
 *   stable adjudication of higher-law disputes) as genuine, but also
 *   identifies asymmetric extraction — legislative majorities and the
 *   electorate's responsiveness function bear a structural cost with no
 *   correction mechanism short of constitutional amendment or generational
 *   judicial turnover. The rising extractiveness trajectory models doctrinal
 *   expansion of judicial review scope over time (courts progressively
 *   extending the domains in which they will second-guess legislative
 *   judgment) without a corresponding expansion of democratic correction
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - constitutional_court_judiciary: institutional/arbitrage — sets and enforces final interpretive authority, self-referentially secured
 *   - rights_claimants: powerless/trapped — primary intended beneficiary of judicial gatekeeping
 *   - legislative_majorities: powerful/constrained — bears nullification cost with no override
 *   - democratic_electorate: organized/constrained — responsiveness function capped by judicial tolerance
 *   - entrenched_minority_interests: organized/arbitrage — secondary beneficiary using judicial review as strategic second veto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '9151b147-682a-4b22-b90a-41f50f4a11af').
narrative_ontology:cs_kernel_codification('9151b147-682a-4b22-b90a-41f50f4a11af', fixed_text).
narrative_ontology:cs_authority_grounding('9151b147-682a-4b22-b90a-41f50f4a11af', lineage).
narrative_ontology:cs_interpretation_layer_present('9151b147-682a-4b22-b90a-41f50f4a11af').
narrative_ontology:cs_reading_relation('9151b147-682a-4b22-b90a-41f50f4a11af', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('9151b147-682a-4b22-b90a-41f50f4a11af', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('9151b147-682a-4b22-b90a-41f50f4a11af', foundational, judicial_finality_over_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_over_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('9151b147-682a-4b22-b90a-41f50f4a11af', judicial_finality_over_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('9151b147-682a-4b22-b90a-41f50f4a11af', secondary, counter_majoritarian_rights_protection_requires_insulated_arbiter).
narrative_ontology:cs_axiom_status(counter_majoritarian_rights_protection_requires_insulated_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('9151b147-682a-4b22-b90a-41f50f4a11af', counter_majoritarian_rights_protection_requires_insulated_arbiter, instrumental).
narrative_ontology:cs_reference_frame('9151b147-682a-4b22-b90a-41f50f4a11af', founding_era_rights_protective_review).
narrative_ontology:cs_drift_state('9151b147-682a-4b22-b90a-41f50f4a11af', contemporary_judicial_activism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9151b147-682a-4b22-b90a-41f50f4a11af', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, entrenched_minority_interests).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on whether legislation stands or falls against the constitutional text. Sets the interpretive doctrine (originalism, purposive reading, proportionality tests) that determines outcomes, and no other body can overrule its constitutional determinations short of formal amendment. Its own authority is self-referentially secured by the same interpretive supremacy it exercises.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary, beneficiary).

% Individuals and minorities whose rights would be vulnerable to majoritarian legislative action absent judicial review. They petition courts to strike down laws that burden them and depend entirely on the judiciary's willingness to exercise this gatekeeping function; they have no legislative recourse if courts decline to act, but benefit decisively when courts do.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, biographical, trapped, national).

% Enact statutes reflecting current electoral majorities, which can be nullified by judicial invalidation with no override mechanism available. They can attempt constitutional amendment (a far higher bar than ordinary legislation) or attempt to reshape the judiciary through appointments over time, but in the interval between elections and appointment cycles they bear the cost of having their enacted preferences voided by an unelected body.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, biographical, constrained, national).

% Votes for representatives whose enacted policy can be reversed by judicial ruling regardless of electoral mandate. Its responsiveness function — translating majority will into binding law — is structurally capped by whatever the courts will tolerate under their reading of the text. Exit requires either constitutional amendment supermajorities or generational judicial appointment shifts, both slow relative to the electorate's own time horizon.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_electorate, payer,
    organized, generational, constrained, national).

% Well-resourced interest groups (property owners, incumbent industries, established religious or political factions) that litigate strategically to have unfavorable legislation struck down as unconstitutional, using judicial supremacy as a second veto point after losing in the legislature. They benefit from the same gatekeeping function that protects powerless rights-claimants, but with far greater capacity to invoke it repeatedly.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, entrenched_minority_interests, beneficiary,
    organized, civilizational, arbitrage, national).

% The historical framers whose original intent or text is invoked as the source of judicial authority are not present to arbitrate between competing readings of what they granted; courts speak for them without the possibility of correction or dissent from the drafters themselves.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_drafters_historical, excluded,
    analytical, civilizational, analytical, national).

% Study cross-jurisdictional variation in interpretive supremacy arrangements, comparing outcomes under judicial supremacy against legislative override and popular sovereignty models to assess democratic responsiveness costs and rights-protection benefits.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, insulated forum for resolving disputes about the meaning of higher law, preventing each legislative majority from simply re-legislating around constitutional limits, and giving rights-claimants a durable point of appeal that does not depend on winning the next election.
% TRANSFER_FUNCTION: Moves final interpretive authority — and with it, the practical power to nullify enacted legislation — from elected legislatures and the electorate to appointed or life-tenured judges; moves protective certainty to rights-claimants and litigation-capable minority interests at the cost of legislative majorities' ability to have their enactments stand.
% ABSENT_VOICES: The historical constitutional drafters cannot arbitrate between judicial supremacy, legislative sovereignty, and popular sovereignty readings of their own text; legislative majorities whose statutes are invalidated have no formal voice within the judicial proceeding beyond litigation counsel, and the diffuse electorate whose mandate is overridden is not a party to the case at all.
% DISAPPEARANCE_RATIONALE: If judicial supremacy disappeared overnight and courts could no longer conclusively invalidate legislation, legislatures would face no binding constitutional ceiling beyond self-restraint or political cost; rights-claimants would lose their most durable point of appeal; strategic litigation by entrenched interests as a second veto point would collapse; constitutional politics would shift toward legislative override mechanisms or popular constitutional conventions.
% FOUNDING_PROBLEM: Constitutions were adopted in part to prevent transient legislative majorities from eroding foundational rights or restructuring government in ways that entrench their own power — the founding problem is majoritarian overreach against minorities and against the constitutional structure itself.
% FOUNDING_PROBLEM_CORROBORATION: Rights-claimant advocacy groups and the judiciary itself attest the problem remains live, citing ongoing majoritarian threats to minority rights. Legislative-sovereignty scholars and comparative constitutional theorists (e.g., critiques from Waldron and others outside the judiciary and outside rights-litigation constituencies) attest that the mechanism has drifted from protecting minorities toward entrenching judicial policy preferences and elite litigant advantage, a status the benefiting parties dispute.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects genuine but partial asymmetric extraction: courts perform a real coordination function (stable rights protection against majoritarian churn) while simultaneously extracting decision authority from democratic institutions without a correction channel proportionate to the power taken. Suppression (0.58) is elevated because legislative override is structurally impossible under this reading — the only exits are supermajority constitutional amendment or multi-decade appointment strategy, both high-friction. Theater ratio is comparatively low (0.22) because the interpretive function is substantially real, not primarily performative, though it rises modestly as doctrinal expansion (broader standing, wider rights catalogs, more aggressive proportionality review) increasingly displaces legislative judgment beyond the original rights-protective core. Accessibility collapse (0.68) is high because once judicial supremacy is textually and doctrinally entrenched, legislatures genuinely cannot route around it — this is a structural, not merely rhetorical, foreclosure of alternatives. Resistance (0.55) reflects sustained legislative-sovereignty and popular-constitutionalism critique from scholars and periodic court-curbing political movements.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, judicial supremacy is neutral guardianship of higher law against transient political passion. From the legislative-majority seat, the identical structure is an unelected body's veto with no symmetric check. The engine should compute these as structurally different experiences of the same arrangement — the judiciary's institutional/arbitrage position and the legislature's powerful/constrained position diverge sharply in effective extraction despite both being nominally powerful actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-claimants and entrenched minority interests are declared beneficiaries because the constraint's operation directly protects or advances their positions without requiring them to run the apparatus — they invoke judicial review, they do not administer it. The judiciary itself is both agenda-setter and beneficiary: it collects institutional authority and legitimacy from being the conclusive interpreter, and no external body can correct its exercise of that role. Legislative majorities and the democratic electorate are victims: the transfer function moves final say over legislation's fate away from the body that enacted it and the constituency that elected it, into a body they cannot recall or overrule within any realistic time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting minorities and constitutional structure from majoritarian overreach — remains genuinely live in many cases, which is why this reading is authored as tangled_rope rather than snare: the coordination function is real, not merely cover. But the corroboration split (judiciary and rights-advocates say live; legislative-sovereignty scholars and comparative theorists outside the beneficiary set say the mechanism has drifted toward entrenching judicial policy preference and elite litigant advantage) signals partial mandatrophy risk requiring ongoing scrutiny rather than a settled verdict either way. The classification prevents mislabeling this as pure extraction (ignoring the real minority-protection function) or as pure coordination (ignoring the uncorrectable transfer from democratic majorities).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_legislative_sovereignty_framing,
    'Is the correct reading of the constitutional text judicial supremacy (courts conclusively determine meaning), legislative sovereignty (parliament retains final say), or popular sovereignty (the demos alone holds ultimate interpretive authority)? This story generates the judicial supremacy reading as one instantiation among three live, mutually competing readings of the same kernel text.',
    'No single empirical test resolves this — it is a foundational question of constitutional design philosophy. Partial evidence comes from comparative outcomes: jurisdictions with override mechanisms (legislative sovereignty) versus pure judicial review (judicial supremacy) versus amendment-heavy popular constitutionalism can be compared for rights-protection efficacy and democratic responsiveness over multi-decade windows.',
    'If the legislative_sovereignty_reading or popular_sovereignty_reading is adopted instead, the beneficiary/victim structure inverts substantially: legislative majorities and the electorate become beneficiaries of retained final say, while rights-claimants lose their most durable point of appeal and become more exposed to majoritarian action. Each reading is authored as its own constraint with its own ε rather than blended into this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_legislative_sovereignty_framing, conceptual, 'Kernel-level indeterminacy: which reading of constitutional interpretive authority is correct — routed here rather than blended into base_properties.').

omega_variable(
    doctrinal_scope_creep_ambiguity,
    'Does the rising extractiveness trajectory reflect genuine doctrinal expansion beyond the founding rights-protective function, or does it reflect legitimate adaptation of constitutional principles to new circumstances the framers could not have anticipated?',
    'Doctrinal history analysis tracing specific expansions of judicial review scope (standing rules, level of scrutiny, subject-matter reach) against whether each expansion tracks a genuinely new rights threat or an assertion of judicial policy preference into previously legislative domains.',
    'If scope creep is genuine adaptation, the rising extractiveness measurement overstates drift toward extraction; if it is policy substitution, the measurement understates it and mandatrophy risk is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_scope_creep_ambiguity, empirical, 'Whether rising extraction reflects legitimate doctrinal adaptation or judicial policy substitution for legislative judgment.').

omega_variable(
    judicial_selection_mechanism_dependency,
    'Does the extraction profile of judicial supremacy depend heavily on how judges are selected (life tenure and insulated appointment vs. periodic election vs. term limits), such that the same textual grant of interpretive authority produces very different effective extraction depending on selection mechanism?',
    'Cross-jurisdictional comparison of judicial-supremacy systems with different selection and tenure mechanisms, measuring divergence between elected-judiciary and appointed-life-tenure judiciary systems on the same extractiveness and suppression metrics.',
    'If extraction is highly selection-mechanism-dependent, this story''s metrics should be understood as specific to insulated/life-tenure judicial supremacy rather than to judicial supremacy as a kernel reading generally, which would argue for further decomposition by selection mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_selection_mechanism_dependency, empirical, 'Whether judicial selection/tenure mechanism materially changes the extraction profile within the judicial supremacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t12, constitutional_text__judicial_supremacy_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__judicial_supremacy_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(cons_tr_t36, constitutional_text__judicial_supremacy_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(cons_tr_t48, constitutional_text__judicial_supremacy_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__judicial_supremacy_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t12, constitutional_text__judicial_supremacy_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(cons_be_t24, constitutional_text__judicial_supremacy_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(cons_be_t36, constitutional_text__judicial_supremacy_reading, base_extractiveness, 36, 0.47).
narrative_ontology:measurement(cons_be_t48, constitutional_text__judicial_supremacy_reading, base_extractiveness, 48, 0.5).
narrative_ontology:measurement(cons_be_t60, constitutional_text__judicial_supremacy_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t12, constitutional_text__judicial_supremacy_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(cons_su_t24, constitutional_text__judicial_supremacy_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(cons_su_t36, constitutional_text__judicial_supremacy_reading, suppression_requirement, 36, 0.53).
narrative_ontology:measurement(cons_su_t48, constitutional_text__judicial_supremacy_reading, suppression_requirement, 48, 0.56).
narrative_ontology:measurement(cons_su_t60, constitutional_text__judicial_supremacy_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Part of the constitutional_text kernel family (3 readings): this story (judicial_supremacy_reading), legislative_sovereignty_reading, and popular_sovereignty_reading. Each reading is authored as a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle — the natural-language label 'who has final constitutional authority' conflates three incompatible institutional arrangements. This reading's ε (0.52, tangled_rope) reflects genuine but partial asymmetric extraction from legislative majorities toward rights-claimants and the judiciary itself; the legislative_sovereignty_reading is expected to show a materially different beneficiary structure (legislative majorities and the electorate as beneficiaries) and the popular_sovereignty_reading a different one still (constituent power / amendment-capable publics as beneficiaries, with both courts and legislatures as constrained agenda-setters rather than final arbiters). This reading forecloses the legislative_sovereignty_reading within a single legal framework (a court cannot simultaneously hold and lack final say over the same statute) but coexists with the popular_sovereignty_reading, since popular constituent power (amendment) can in principle sit above judicial supremacy without contradicting it in the ordinary case.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
