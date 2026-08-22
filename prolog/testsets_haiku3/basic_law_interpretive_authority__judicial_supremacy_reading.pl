% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/institutional_design
 *
 * SUMMARY:
 *   The judicial supremacy reading of basic law interpretive authority claims
 *   courts hold final power to determine what the constitution means through
 *   specialized expertise and insulation from electoral pressure. This
 *   reading constructs the judiciary as the essential protector against
 *   majoritarian tyranny and the sole actor capable of neutral constitutional
 *   interpretation. Legislatures and electoral majorities experience this
 *   arrangement as constraint: their laws can be invalidated by unelected
 *   judges, and amendment of that authority requires supermajority consensus
 *   that courts may still challenge. The reading instantiates the constraint
 *   as TANGLED ROPE: genuine coordination function (settling interpretive
 *   disputes through neutral arbitration, preventing legislative
 *   self-judging) coupled with asymmetric extraction (judicial authority over
 *   legislative scope, costs borne by blocked majorities). The measurement
 *   series tracks suppression intensification from t=0 to t=32 (rising
 *   enforcement machinery protecting judicial review power from legislative
 *   override attempts, including court-blocking proposals and amendment
 *   challenges) followed by slight decline at t=40-50 as political attention
 *   shifts and enforcement maintenance becomes routine.
 *
 * KEY AGENTS:
 *   - judiciary_as_interpreter: Institutional agenda-setter (powerful, independent, protected from electoral consequence) — sets and enforces the supreme interpretive authority. Beneficiary from concentrated authority.
 *   - legislative_majorities: Institutional payer (organized, constrained exit) — bears gridlock cost and policy loss when laws face judicial invalidation. Targeted by the supremacy constraint.
 *   - electoral_majorities: Organized payer (constrained exit through supermajority amendment requirements) — their democratic choice constrained by non-elected judicial review. Targeted asymmetrically.
 *   - individual_rights_claimants: Powerless beneficiary (arbitrage exit if they lose cases) — benefit from judicial protection against majoritarian legislation and individual rights violation.
 *   - legal_interpreters_outside_courts: Excluded observers (scholars, legislators, citizens offering constitutional readings) — structurally kept from final interpretive authority. Their exclusion is the enforcement object.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '63eeec70-b043-486f-8ecc-c23c4f29761e').
narrative_ontology:cs_kernel_codification('63eeec70-b043-486f-8ecc-c23c4f29761e', formalized).
narrative_ontology:cs_authority_grounding('63eeec70-b043-486f-8ecc-c23c4f29761e', lineage).
narrative_ontology:cs_interpretation_layer_present('63eeec70-b043-486f-8ecc-c23c4f29761e').
narrative_ontology:cs_reading_relation('63eeec70-b043-486f-8ecc-c23c4f29761e', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('63eeec70-b043-486f-8ecc-c23c4f29761e', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('63eeec70-b043-486f-8ecc-c23c4f29761e', foundational, courts_possess_specialized_constitutional_expertise).
narrative_ontology:cs_axiom_status(courts_possess_specialized_constitutional_expertise, holdable).
narrative_ontology:cs_axiom_grounding('63eeec70-b043-486f-8ecc-c23c4f29761e', courts_possess_specialized_constitutional_expertise, instrumental).
narrative_ontology:cs_axiom('63eeec70-b043-486f-8ecc-c23c4f29761e', foundational, judicial_independence_produces_neutral_interpretation).
narrative_ontology:cs_axiom_status(judicial_independence_produces_neutral_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('63eeec70-b043-486f-8ecc-c23c4f29761e', judicial_independence_produces_neutral_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('63eeec70-b043-486f-8ecc-c23c4f29761e', judicial_constitutional_supremacy).
narrative_ontology:cs_drift_state('63eeec70-b043-486f-8ecc-c23c4f29761e', contemporary_democratic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('63eeec70-b043-486f-8ecc-c23c4f29761e', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_as_interpreter).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, individual_rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, court_system_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and maintains final interpretive authority through review power, precedent setting, and docket control. Sets constitutional boundaries that legislatures must respect. Justifies this through expertise in legal interpretation and institutional independence from majoritarian pressure. Resists legislative override attempts, court-packing proposals, and constitutional amendments that would restrict review power. Acts as the sole final arbiter of what the constitution means.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_as_interpreter, agenda_setter,
    institutional, generational, analytical, national).

% Enacts legislation in good-faith belief it is constitutional, but face judicial invalidation that nullifies the law and frustrates their policy agenda. Must operate within judge-drawn boundaries or risk having laws struck down on remand. Can only override through constitutional amendment, which requires supermajority consensus and faces risk of judicial challenge to the amendment itself. Pays the cost of gridlock when courts block laws, political cost of defending invalidated policies, and the constraint of operating under limits not of their making.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majorities, payer,
    institutional, biographical, constrained, national).

% Votes for representatives whose policies are struck down by courts. Bears the cost of policy non-implementation, delayed gratification of their electoral choice, and frustration of having their will overridden by unelected judges. Can attempt constitutional amendment to restore power, but amendment requires supermajority consensus and uncertain judicial receptivity. Exit is constrained: they cannot easily change the system without extraordinary consensus.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Benefit when courts strike down laws as unconstitutional violations of their rights. Gain judicial protection against majoritarian legislation, a neutral forum to appeal government action, and binding remedies if they prevail. Stakes are concrete: a constitutional victory can mean freedom, liberty, or compensation. Can exit by accepting lower-court judgment or moving jurisdictions; losing in court does not prevent appeal or relocation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, individual_rights_claimants, beneficiary,
    powerless, biographical, arbitrage, national).

% Constitutional amendment requires achieving supermajority consensus across states and both chambers. Even commanding this supermajority does not guarantee the amended constitution will not be challenged or reinterpreted by courts. Sit at the margin of constitutional authority but remain constrained by judicial review of the amendment process itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_supermajorities, observer,
    organized, generational, constrained, national).

% Scholars, legislators, and citizens who produce constitutional interpretations are formally excluded from final authority. Their views are relevant only if courts choose to adopt them. Would expand interpretive space and democratize constitutional meaning-making if they held equal authority. Structurally kept out by the supremacy claim itself; their inclusion would undermine judicial supremacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_interpreters_outside_courts, excluded,
    moderate, generational, trapped, national).

% Judges set the agenda through docket control, breadth/narrowness of rulings, and precedential scope. Benefit from concentrated institutional authority, the ability to shape doctrine over generations, and protection of that authority from legislative challenge. Administer tenure and compensation protections that insulate them from electoral consequence. Maintain the interpretive monopoly through institutional practice and precedent.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, court_system_administration, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, court_system_administration, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_as_interpreter).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative venue for resolving disputes about constitutional meaning instead of allowing each political actor to claim unilateral interpretive power. Prevents constitutional chaos by concentrating final say in a body insulated from pressure to reverse itself repeatedly. Enables individuals to appeal to a neutral arbiter when their rights conflict with majoritarian legislation.
% TRANSFER_FUNCTION: Transfers interpretive power from the legislature and electoral majorities (who must compete for votes) to the judiciary (who are shielded from electoral consequence). Moves policy authority from legislative process to judicial process. Redistributes the cost of invalidating popular legislation from the invalidating actor (courts) to the invalidated people (the legislature and voters who supported the law).
% ABSENT_VOICES: Non-judicial constitutional interpreters — legislators, scholars, citizens engaged in constitutional self-interpretation — would argue that democratic constitutional dialogue distributes interpretive authority more robustly and preserves legislative deliberation as a coordinate constitutional power. Judicial review advocates are amplified; popular or parliamentary voices are structured out of final decision.
% DISAPPEARANCE_RATIONALE: If judicial supremacy disappeared overnight, legislatures would immediately acquire unilateral power to interpret the constitution as constraining their own conduct, abolishing judicial review and rewriting constitutional meaning through legislation. Rights protections would depend entirely on legislative goodwill. The separation of powers would collapse into legislative sovereignty. Constitutional governance would reorganize around the legislature as final interpreter.
% FOUNDING_PROBLEM: Post-ratification disputes about constitutional meaning produced deadlock when the legislature was the sole authoritative interpreter; Federalists feared majoritarian violation of minority rights; Marbury v. Madison (1803) established courts as arbiters to prevent tyranny of the majority and settle interpretive disputes through neutral legal reasoning rather than political pressure.
% FOUNDING_PROBLEM_CORROBORATION: Judicial supremacy advocates (sitting judges, Federalist-tradition scholars) testify the founding problem — legislative self-judging and majority tyranny — remains live. Democratic and parliamentary sovereignty advocates (elected officials, popular constitutionalism scholars, legislative-history analysts) testify the founding problem was solved by electoral accountability and that judicial supremacy now creates its own tyranny: judicial override of democratic choice. Independence analysts from outside the benefiting parties (institutional economists, comparative constitutional scholars) document both functions present and the structural divergence in how different seats experience the constraint.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers final interpretive authority from the elected branches to an unelected body, whose composition and ideology diverge from electoral majorities. The cost of judicial override (policy invalidation, amendment blockage, gridlock) exceeds the cost of neutral interpretation; this gap widens when courts adopt narrow doctrines that protect their authority from legislative override. Suppression measures enforcement intensity: courts must actively resist congressional court-packing, jurisdiction-stripping, and amendment attempts to maintain the supremacy claim. Suppression rises from 0.48 to peak at 0.72 during periods of intense political conflict over judicial power (civil rights era, recent polarization), then slightly declines as enforcement becomes institutionalized and opposition is exhausted. Theater rises from 0.12 to 0.29: judicial rhetoric about 'neutral interpretation' and 'restraint' intensifies as suppression rises, but the actual operation (blocking legislatures, controlling docket, choosing precedential scope) becomes more explicitly political. The measurement grid is shared across all three metrics, with every point authored at every time step, capturing the temporal evolution of the constraint over 50 years.
 *
 * PERSPECTIVAL GAP:
 *   The critical divergence sits between the judiciary's seat and the legislative/electoral majority seats. The courts claim they are neutral interpreters protecting rights; legislative majorities claim courts are veto-players extracting authority from democratic process. Both cannot be true as descriptions of what is happening: either courts are providing coordination against majoritarian tyranny, or they are extracting from democratic majorities, or both functions coexist. The engine computes per-seat classification from the structural data: the same constraint produces 'rope' or 'genuine coordination' from the judiciary's perspective, and 'snare' or 'pure extraction' from the constrained majority's perspective. The authored metrics (high extractiveness, high suppression, rising theater) describe the constraint as it operates, not as it is experienced; the per-seat computation reveals how fundamentally different seats read the same institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   From the judiciary's seat (d ≈ 0.0): the constraint is genuine coordination that the courts built and maintain through expertise and independence — they are net beneficiaries and experience it as legitimate authority. From the legislative majority seat (d ≈ 0.95): the constraint is extraction through judicial override of democratic choice — they are net targets and experience it as imposed limitation. From the electoral majority seat (d ≈ 0.88): the constraint blocks their policy will when courts strike laws they elected representatives to enact — they are targeted through the legislative constraint. From the individual rights claimant seat (d ≈ 0.15): the constraint is coordination that protects them from majoritarian legislation — they benefit. This perpectival divergence is the measurement point: each seat computes a different type from identical structural data because the constraint's directionality differs per seat. Suppression is pure constraint (unscaled): the actual enforcement machinery courts deploy to maintain judicial review power against legislative challenge. Extractiveness is scaled by directionality: for the judiciary (low d) it's dampened into subsidy; for majorities (high d) it's amplified into extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative self-judging, majoritarian tyranny) remains CONTESTED. Judicial supremacy advocates attest it is still live; democratic sovereignty advocates attest it has been solved by electoral accountability and that supremacy now creates its own tyranny (unaccountable judicial override). This contestation prevents mandatrophy classification: the constraint is not dead because courts genuinely do perform the coordination function (settle interpretive disputes, prevent constitutional chaos), but it is not fully alive because the original majoritarian-tyranny problem is disputed. The classification as TANGLED ROPE (not SNARE) depends on accepting that both coordination and extraction are structurally present: courts do coordinate interpretation, and they do extract authority from democratic process through judicial review. If the coordination component collapses (if courts begin behaving as pure political actors), the constraint would reclassify to snare. If the extraction component collapses (if amendment power truly becomes unrestricted), it would reclassify to rope. The measurement of theater_ratio (rising from 0.12 to 0.29) tracks this risk: as judicial defense of supremacy becomes more openly political, the performance of neutrality increases, suggesting the coordination function is increasingly contested and suppression-dependent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_vs_epistemic_authority,
    'Does specialized legal expertise justify final interpretive authority, or does it justify only advisory institutional role?',
    'Empirical examination of judicial track record: do judges outperform legislatures and democratic interpreters in achieving fidelity to constitutional text, stable meaning over time, and protection of enduring rights? Do they demonstrate fewer reversals, less drift into political ideology, and more consistent principle than legislative interpretation?',
    'If judges demonstrably outperform on these metrics, expertise justifies some supremacy; if performance is equivalent or legislatures outperform, supremacy is unjustified extraction riding on a false expertise claim. This would reclassify from tangled_rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expertise_vs_epistemic_authority, empirical, 'Whether judicial expertise actually grounds superior constitutional interpretation or merely legitimates authority.').

omega_variable(
    independence_mechanism,
    'Does judicial independence from electoral pressure actually produce neutral interpretation, or does it produce insulation from accountability that permits ideological drift?',
    'Long-term doctrinal analysis: do protected judges'' interpretations shift less with personnel changes, or shift just as much as accountable officials'' do? Do they resist political pressure or incorporate it without public debate? Do they self-correct when wrong, or entrench through precedent?',
    'If independence produces stable neutrality, it justifies the coordination claim; if independence permits unaccountable ideological drift, it undermines the expertise justification and reveals the suppression as protection of authority rather than protection of rights. This would shift the constraint toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_mechanism, empirical, 'Whether judicial independence enables neutral interpretation or enables unaccountable ideological authority.').

omega_variable(
    majoritarian_tyranny_diagnosis,
    'Is the original founding problem (majoritarian violation of minority rights) actually solved by judicial supremacy, or does majoritarian tyranny continue in different forms?',
    'Rights-protection metrics: compare judicial-era rights protections against pre-judicial and post-amendment eras. Do minorities suffer majoritarian persecution less often? Are rights-claims by majorities blocked with equal frequency to minority claims? Do courts protect unpopular groups or popular minorities?',
    'If majorities are equally protected and minorities equally empowered, coordination exists; if protection correlates with political alignment, extraction is revealed. This determines whether the constraint genuinely solves the founding problem or merely redistributes it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_tyranny_diagnosis, empirical, 'Whether judicial supremacy actually prevents majoritarian tyranny or produces different forms of it.').

omega_variable(
    kernel_reading_divergence,
    'Does the basic_law_interpretive_authority kernel admit genuinely incompatible readings, or are the readings merely different emphasis on elements of a single true arrangement?',
    'Can one institutional actor hold final interpretive authority in the same constitutional framework that another actor holds it? Can legislative supremacy and judicial supremacy coexist as commitments, or is supremacy singular and the readings genuinely incompatible?',
    'If readings foreclose each other (one actor''s supremacy precludes another''s in the same framework), one reading is false and the constraint should be reclassified or eliminated. If readings coexist (different countries or different parties claiming different supremacy simultaneously), the kernel is genuinely contested and classification depends on which reading the story instantiates. This determines whether the committer structure is a real constitutional disagreement or a terminological confusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Whether judicial supremacy reading and parliamentary sovereignty reading genuinely foreclose each other or coexist in different constitutional traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, amendment_supermajority_requirement).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, separation_of_powers_institutional_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_authority kernel. Sibling readings (parliamentary_sovereignty, popular_constitutionalism) each instantiate different constraints from the same kernel, with different beneficiary/victim structures and different ε values. The three stories form a constraint family linked by network.affects_constraints: judicial supremacy forecloses (logically rules out) parliamentary supremacy in any single framework, and influences (changes legitimacy conditions for) popular constitutionalism. Each reading is authored as a clean ε-invariant constraint to itself; the kernel contest is routed through omega variables and cs_structure fields rather than embedded in the narratives themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__judicial_supremacy_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
