% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living-Constitution Interpretive Arrangement (Adaptive Reading of the U.S. Constitutional Text)
 *   domain: legal/political
 *
 * SUMMARY:
 *   In United States constitutional practice, the dominant 20th-century
 *   interpretive arrangement treats the Constitution's broad principles as
 *   adaptable: meaning evolves with society, and judges apply principles to
 *   contemporary circumstances. This story instantiates the
 *   LIVING-CONSTITUTIONALIST READING of the us_constitution_text kernel as a
 *   clean, epsilon-invariant constraint; the originalist and positivist
 *   readings are separate constraints in separate files, linked through
 *   network edges. The epsilon referent is the standing adaptive-interpretive
 *   arrangement as this reading assesses it: a genuine coordination
 *   achievement — keeping a 1787 charter operative across transformed
 *   conditions — carrying a real but bounded cost, namely interpretive
 *   authority concentrated in an unelected judiciary and paid by democratic
 *   majorities, state governments, and fixed-meaning claimants. Claim and
 *   metrics are independent authored facts: claimed_type tangled_rope records
 *   this reading's structural self-understanding (real coordination function
 *   plus real asymmetric payment through the same structure); the metrics
 *   record descriptive best estimates of how the arrangement actually
 *   operates. KEY AGENTS (by structural relationship): - federal_judiciary:
 *   agenda_setter (institutional/identity_locked) — administers adaptive
 *   interpretation through judicial review and stare decisis; collects
 *   interpretive authority - rights_claimants_changed_contexts: primary
 *   beneficiary (powerless/constrained) — obtains protections under evolved
 *   meanings - democratic_majorities_overridden: primary payer
 *   (organized/constrained) — loses policy questions to judicial settlement -
 *   state_legislatures_invalidated: payer (organized/constrained) —
 *   enactments fall under evolved equal-protection and due-process readings -
 *   fixed_meaning_advocates: payer (powerful/mobile) — originalist scholars,
 *   litigants, and citizens whose fixed-meaning claims are displaced by the
 *   standing arrangement - constitutional_law_academy: observer
 *   (moderate/analytical) - future_citizens_bound_by_precedent: excluded
 *   (non-agent seat) — bound by precedents set before they could participate
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.35).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.25).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living-Constitution Interpretive Arrangement (Adaptive Reading of the U.S. Constitutional Text)").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '35516ecd-8c57-4f05-91ae-f3cf284714ec').
narrative_ontology:cs_kernel_codification('35516ecd-8c57-4f05-91ae-f3cf284714ec', fixed_text).
narrative_ontology:cs_authority_grounding('35516ecd-8c57-4f05-91ae-f3cf284714ec', practice).
narrative_ontology:cs_interpretation_layer_present('35516ecd-8c57-4f05-91ae-f3cf284714ec').
narrative_ontology:cs_reading_relation('35516ecd-8c57-4f05-91ae-f3cf284714ec', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('35516ecd-8c57-4f05-91ae-f3cf284714ec', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('35516ecd-8c57-4f05-91ae-f3cf284714ec', foundational, constitutional_meaning_adapts_to_contemporary_circumstances).
narrative_ontology:cs_axiom_status(constitutional_meaning_adapts_to_contemporary_circumstances, holdable).
narrative_ontology:cs_axiom_grounding('35516ecd-8c57-4f05-91ae-f3cf284714ec', constitutional_meaning_adapts_to_contemporary_circumstances, instrumental).
narrative_ontology:cs_axiom('35516ecd-8c57-4f05-91ae-f3cf284714ec', secondary, past_generations_cannot_bind_present_ones_to_fixed_meanings).
narrative_ontology:cs_axiom_status(past_generations_cannot_bind_present_ones_to_fixed_meanings, holdable).
narrative_ontology:cs_axiom_grounding('35516ecd-8c57-4f05-91ae-f3cf284714ec', past_generations_cannot_bind_present_ones_to_fixed_meanings, deontological).
narrative_ontology:cs_reference_frame('35516ecd-8c57-4f05-91ae-f3cf284714ec', living_framework_of_principles).
narrative_ontology:cs_drift_state('35516ecd-8c57-4f05-91ae-f3cf284714ec', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('35516ecd-8c57-4f05-91ae-f3cf284714ec', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, democratic_majorities_overridden).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, state_legislatures_invalidated).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_advocates).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, substantive_due_process_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, evolving_standards_of_decency_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, incorporation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds itself out, through case-and-controversy jurisdiction, as the working interpreter of the constitutional text. Declares what broad principles mean under present conditions, binds lower courts and states through precedent, and defends that role against political pushback. Individual justices come and go; the institution's authority is fused with the interpretive practice itself — stepping outside it would mean ceasing to be the Supreme Court. Its members currently disagree sharply about whether the adaptive approach should continue.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Individuals and groups seeking constitutional protection for conduct, relationships, or decisions the 18th-century text does not name — patients, same-sex couples, criminal defendants, unpopular speakers. Their practical route to protection runs through federal litigation; no other forum secures constitutional rights against state action. Wins arrive slowly, depend on counsel and standing, and can be undone by later courts.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_contexts, beneficiary,
    powerless, biographical, constrained, national).

% Voters and their representatives who settle contested questions by statute or ballot initiative, then see those settlements invalidated when courts read the text's principles to reach further. Their levers are indirect: electing presidents who nominate judges, pressuring senators, and — rarely achievable — the amendment process.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, democratic_majorities_overridden, payer,
    organized, biographical, constrained, national).

% State governments whose policy choices — on marriage, education, policing, morality regulation — are struck down under nationally evolved readings of equal protection and due process. Bound by supremacy; their recourse is litigation, lobbying for sympathetic nominees, and a formal amendment process they cannot realistically trigger alone.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, state_legislatures_invalidated, payer,
    organized, generational, constrained, regional).

% Scholars, lawyers, litigants, and citizens who hold that the text's meaning was fixed when adopted and that departures from that meaning lack democratic warrant. Long outside the interpretive mainstream, they built durable alternative infrastructure — societies, journals, nomination pipelines — and now hold significant institutional leverage. They can pursue their program through appointments and scholarship regardless of what the courts currently do.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, fixed_meaning_advocates, payer,
    powerful, generational, mobile, national).

% Law professors, historians, and commentators who map the interpretive contest, test each methodology against the cases, and train the next generation of clerks and judges. Observes and critiques; decides nothing.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_law_academy, observer,
    moderate, biographical, analytical, national).

% People not yet born or not yet enfranchised who will live under precedents set now. They take no part in today's rulings but inherit their binding force. Kept as a narrative-completeness seat rather than a participating actor.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, future_citizens_bound_by_precedent, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(us_constitution_text__living_constitutionalist_reading, future_citizens_bound_by_precedent).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a brief, old, deliberately hard-to-amend charter operable as supreme law across radically changed conditions: it lets 1787 language govern industrialization, mass media, electronic surveillance, and new family forms without an Article V amendment for each development, and gives officials and citizens a stable method for resolving what the text permits.
% TRANSFER_FUNCTION: Moves ultimate decision authority over contested social questions from enacting majorities and state governments to federal courts; moves enforceable rights protections to litigants who obtain them under evolved meanings rather than through the amendment process.
% ABSENT_VOICES: The adopting generation cannot appear — the dead cannot litigate — and ordinary voters enter only indirectly, through appointment politics. Citizens who would settle contested moral questions legislatively, and state governments defending their own settlements, object from outside the courtroom where the method is applied.
% DISAPPEARANCE_RATIONALE: If adaptive interpretation vanished overnight, the large body of doctrine resting on evolved meanings — incorporation of the Bill of Rights against the states, the anti-discrimination reading of equal protection, the privacy line — would lose its warrant simultaneously; hundreds of precedents would become unstable, state and federal law would lurch into re-litigation of settled questions, and the system would reorganize around either recovered original meaning or a wave of amendment attempts the Article V process cannot absorb.
% FOUNDING_PROBLEM: A short 18th-century charter written in broad principles could not anticipate the society it would govern; without a method for applying its principles to unforeseen conditions, the text would either freeze governance in 1791 categories or require constant amendment the supermajority process makes impractical. Early interpreters adopted adaptive construction so the instrument could, in Marshall's phrase, endure for ages to come.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: originalist scholars concede the text-society distance — their entire project is a response to it, disputing the remedy rather than the gap; legal historians across methodologies document that the framers and early commentators anticipated change (ratification debates, McCulloch v. Maryland, Jefferson's letters on intergenerational obligation). No serious participant in the contest denies the founding problem exists; the dispute is over the solution.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.35: the arrangement transfers real decision authority from elected bodies to courts — a cost this reading itself acknowledges as the counter-majoritarian price — but the reading regards most of that transfer as the purchase price of governability, not rent. Suppression 0.25: adaptive interpretation suppresses almost nothing — originalism competes openly, amendment remains formally available, and coercion is confined to binding precedent on case losers; suppression is authored as a raw structural property and is not scaled by power or scope anywhere in the computation. Theater 0.15: the practice performs real doctrinal work; rhetorical performance ('evolving standards') is a minor fraction. Accessibility collapse 0.25: understanding the arrangement does not close off alternatives — rival methodologies remain fully available, which is precisely why the contest persists. Resistance 0.55: an organized originalist counter-movement, recurring political backlash, and explicit recent repudiations of evolved-meaning reasoning impose continuous pressure. The measurement series run on ONE shared time grid (T=0 approximates 1953, the opening of the Warren Court era; T=72 approximates 2025; one unit ≈ one year), with every tracked metric authored at every point. The suppression_requirement series is non-monotonic by design: enforcement demand ratcheted upward as rights-revolution rulings drew defiance and jurisdiction-stripping threats (culture-war cycle: ruling, backlash, partial settlement, accumulation), then decayed as the coalition sustaining the arrangement lost control of the bench — enforcement-capacity change is the dynamic this story traces, which is why the series is authored at all.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. From the agenda_setter seat (federal_judiciary), the arrangement is the institution's own craft and the source of its authority — coordination it performs and maintains. From the beneficiary seat (rights_claimants_changed_contexts), the same structure is liberation: protections that no amendment process would have delivered in time. From the payer seats (democratic_majorities_overridden, state_legislatures_invalidated, fixed_meaning_advocates), the identical structure is displacement — questions they regard as theirs, settled elsewhere. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive rights_claimants_changed_contexts toward the subsidized end (d near 0.0): the arrangement delivers protections they could obtain nowhere else, and their exit options are poor. Victim declarations drive the three payer seats toward the target end: democratic_majorities_overridden and state_legislatures_invalidated are constrained (supremacy binds them; amendment is out of reach), so their effective extraction is amplified by trap-weighting; fixed_meaning_advocates carry high d despite power and mobility — their structural relationship is target-of-displacement, and their arbitrage-grade exit (parallel institutions, appointment pipelines) moderates what they effectively pay but not their directional position. One override is declared: the federal_judiciary derives from its beneficiary declaration a d near 0.1, but the seat is split — half the current bench rejects the reading outright, and the institution collectively pays legitimacy costs for maintaining it — so d is overridden to 0.25 to reflect a partially captured, partially resisting administrator rather than a pure collector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the gap between a fixed 18th-century text and a changing society — is live, so no mandatrophy resolution is declared. The classification guards against two symmetric mislabels. Calling the arrangement pure coordination ignores the documented authority transfer that democratic and state seats pay through the same structure that protects claimants. Calling it pure extraction ignores the genuine coordination function (an unamendable-in-practice charter made operable) and the absence of suppressed exits — the arrangement's rivals are thriving, not silenced. On the R5 mismatch consumer: founding_problem_status=live combined with disappearance_verdict=world_rearranges produces no zombie flag; the arrangement's persistence tracks its function, and its current vulnerability comes from a rival reading's institutional ascent, not from atrophy of its own purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the us_constitution_text kernel; what structurally changes if a sibling reading (originalist or positivist) is instantiated instead?',
    'Author the sibling readings as separate constraint files per the epsilon-invariance principle; compare computed types, beneficiary sets, and epsilon across the family file-to-file.',
    'Under the originalist reading the beneficiary and victim sets invert (fixed-meaning claimants gain standing, evolved-rights claimants lose judicially secured protections) and the epsilon referent shifts to a different standing arrangement; cross-reading comparisons are valid only between files, never averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame decomposition of the constitutional-text kernel into rival readings.').

omega_variable(
    counter_majoritarian_cost_or_capture,
    'Is the transfer of interpretive authority from elected bodies to federal courts a justified coordination cost of keeping an 18th-century text governable, or capture of democratic authority by an insulated institution?',
    'Comparative institutional analysis across jurisdictions with different interpretive regimes; measure rights protection, doctrinal stability, and democratic responsiveness against interpretive-method variables.',
    'If capture, effective extraction rises and the arrangement trends toward pure extraction; if justified cost, it trends toward pure coordination with the judiciary''s authority treated as overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_cost_or_capture, conceptual, 'Whether the authority transfer embedded in adaptive interpretation is priced coordination or rent.').

omega_variable(
    hollow_hope_beneficiary_ambiguity,
    'Do rights claimants in changed social contexts benefit net from judicially managed adaptation, or does court-centered rights protection substitute for and weaken more durable legislative and movement-based gains?',
    'Longitudinal comparison of rights trajectories pursued through litigation versus legislation and social movements; update court-centrism outcome studies with current data.',
    'If the substitution critique holds, the beneficiary declaration weakens, directionality for rights claimants rises above pure-beneficiary levels, and the arrangement reads more extractive than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hollow_hope_beneficiary_ambiguity, empirical, 'Net beneficiary status of litigation-dependent rights claimants.').

omega_variable(
    standing_arrangement_survival,
    'Will the adaptive-interpretive arrangement survive the current originalist ascendancy, or is it being displaced — and if displaced, what residue persists in lower-court practice and entrenched precedent?',
    'Track appointment pipelines, the overruling trajectory of evolved-meaning precedents, and lower-court citation patterns to adaptive doctrines over the coming decade.',
    'If displacement completes, this constraint becomes historical and residual enforcement may persist mainly as performance in legacy doctrine; if the arrangement holds, the repudiation-pressure drift reverses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_arrangement_survival, empirical, 'Survival prospects of the standing adaptive arrangement under a hostile bench.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_text__living_constitutionalist_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(us_c_tr_t12, observed).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_text__living_constitutionalist_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(us_c_tr_t24, observed).
narrative_ontology:measurement(us_c_tr_t36, us_constitution_text__living_constitutionalist_reading, theater_ratio, 36, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t36, observed).
narrative_ontology:measurement(us_c_tr_t48, us_constitution_text__living_constitutionalist_reading, theater_ratio, 48, 0.13).
narrative_ontology:measurement_basis(us_c_tr_t48, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_text__living_constitutionalist_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).
narrative_ontology:measurement(us_c_tr_t72, us_constitution_text__living_constitutionalist_reading, theater_ratio, 72, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t72, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t12, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement_basis(us_c_be_t12, observed).
narrative_ontology:measurement(us_c_be_t24, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement_basis(us_c_be_t24, observed).
narrative_ontology:measurement(us_c_be_t36, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 36, 0.3).
narrative_ontology:measurement_basis(us_c_be_t36, observed).
narrative_ontology:measurement(us_c_be_t48, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 48, 0.31).
narrative_ontology:measurement_basis(us_c_be_t48, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement_basis(us_c_be_t60, observed).
narrative_ontology:measurement(us_c_be_t72, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 72, 0.35).
narrative_ontology:measurement_basis(us_c_be_t72, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t12, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement_basis(us_c_su_t12, observed).
narrative_ontology:measurement(us_c_su_t24, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 24, 0.26).
narrative_ontology:measurement_basis(us_c_su_t24, observed).
narrative_ontology:measurement(us_c_su_t36, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 36, 0.3).
narrative_ontology:measurement_basis(us_c_su_t36, observed).
narrative_ontology:measurement(us_c_su_t48, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 48, 0.33).
narrative_ontology:measurement_basis(us_c_su_t48, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 60, 0.29).
narrative_ontology:measurement_basis(us_c_su_t60, observed).
narrative_ontology:measurement(us_c_su_t72, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 72, 0.25).
narrative_ontology:measurement_basis(us_c_su_t72, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Constitution requires' decomposes into three structurally distinct readings of one kernel (us_constitution_text). This living-reading story links to the originalist and positivist sibling files via affects_constraints. Upstream/downstream structure: the originalist reading cites the fixed text's enactment pedigree against this reading; this reading cites post-ratification practice lineage (Marshall through the Warren Court) as its warrant. Epsilon differs across the family because each reading assesses a different standing arrangement by its own lights; values are never averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
