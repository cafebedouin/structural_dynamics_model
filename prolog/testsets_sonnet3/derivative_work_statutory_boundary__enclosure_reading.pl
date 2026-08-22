% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Enclosure Reading of the Derivative Work Boundary (Any Expressive Use = Derivative Preparation)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the enclosure reading of the derivative-work
 *   statutory boundary: any use of copyrighted expression in the creation of
 *   new work is treated as preparation of a derivative work, triggering the
 *   exclusive right regardless of transformation, purpose, or market effect.
 *   This is a distinct constraint from the coordination_reading (which
 *   requires substantial incorporation into a fixed recasting before the
 *   right attaches) and the hybrid_carveout_reading (which conditions the
 *   boundary on commercial exploitation). Under the enclosure reading, the
 *   practical effect is that licensing clearance becomes a precondition for
 *   creation itself rather than a remedy triggered by demonstrated market
 *   harm — the licensing/litigation infrastructure captures value from
 *   independent creators, fan communities, and small developers who pose no
 *   plausible substitution threat to the original market. ε here reflects the
 *   enclosure reading's own operation as the standing arrangement under
 *   contest, not the narrower alternative it displaces.
 *
 * KEY AGENTS:
 *   - legacy_rightsholder_conglomerates: institutional beneficiary and agenda-setter, collects licensing/settlement revenue from the broad boundary
 *   - licensing_intermediary_firms: organized beneficiary, revenue scales with breadth of the derivative-preparation standard
 *   - litigation_specialist_law_firms: organized beneficiary, generates fee revenue from enforcement volume
 *   - independent_creators and fan_transformative_communities: powerless payers, self-censor rather than litigate the boundary
 *   - small_ai_and_software_developers, documentary_and_criticism_producers, library_and_archive_institutions: moderate-power payers bottlenecked by preemptive clearance costs
 *   - courts_and_legislatures: institutional observer, the seat that could displace this reading with a narrower one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.79).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Enclosure Reading of the Derivative Work Boundary (Any Expressive Use = Derivative Preparation)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '14afc382-05db-41b4-9248-d80e7641182a').
narrative_ontology:cs_kernel_codification('14afc382-05db-41b4-9248-d80e7641182a', fixed_text).
narrative_ontology:cs_authority_grounding('14afc382-05db-41b4-9248-d80e7641182a', extraction).
narrative_ontology:cs_interpretation_layer_present('14afc382-05db-41b4-9248-d80e7641182a').
narrative_ontology:cs_reading_relation('14afc382-05db-41b4-9248-d80e7641182a', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('14afc382-05db-41b4-9248-d80e7641182a', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('14afc382-05db-41b4-9248-d80e7641182a', foundational, mere_incorporation_triggers_exclusive_right).
narrative_ontology:cs_axiom_status(mere_incorporation_triggers_exclusive_right, holdable).
narrative_ontology:cs_axiom_grounding('14afc382-05db-41b4-9248-d80e7641182a', mere_incorporation_triggers_exclusive_right, conventional).
narrative_ontology:cs_axiom('14afc382-05db-41b4-9248-d80e7641182a', foundational, transformative_purpose_irrelevant_to_threshold).
narrative_ontology:cs_axiom_status(transformative_purpose_irrelevant_to_threshold, holdable).
narrative_ontology:cs_axiom_grounding('14afc382-05db-41b4-9248-d80e7641182a', transformative_purpose_irrelevant_to_threshold, instrumental).
narrative_ontology:cs_reference_frame('14afc382-05db-41b4-9248-d80e7641182a', narrow_substantial_incorporation_common_law_origin).
narrative_ontology:cs_drift_state('14afc382-05db-41b4-9248-d80e7641182a', contemporary_digital_and_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('14afc382-05db-41b4-9248-d80e7641182a', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, legacy_rightsholder_conglomerates).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediary_firms).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, litigation_specialist_law_firms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, fan_transformative_communities).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, small_ai_and_software_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, documentary_and_criticism_producers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, library_and_archive_institutions).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, author_exclusive_control_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, expression_as_fully_enclosable_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds large back catalogs of copyrighted expression and lobbies for the broadest possible reading of what counts as derivative preparation. Because any incorporation of protected expression is treated as derivative-work preparation under this reading, the conglomerate can demand licensing fees or injunctions against nearly any downstream use that touches its catalog, regardless of transformation. It funds enforcement litigation and treats settlement extraction as a predictable revenue line.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, legacy_rightsholder_conglomerates, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, legacy_rightsholder_conglomerates, agenda_setter).

% Operates clearance and licensing infrastructure that becomes mandatory the moment any expressive use is presumptively derivative. Collects transaction fees on every license issued and has no incentive to narrow the boundary that generates its business; its revenue scales directly with how broadly 'derivative preparation' is read.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediary_firms, beneficiary,
    organized, generational, arbitrage, global).

% Represents rightsholders in enforcement actions and collects fees regardless of whether the underlying use caused any market harm. A broad derivative-preparation standard maximizes the volume of colorable claims it can bring or threaten, generating settlement pressure even against uses with strong transformative arguments.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, litigation_specialist_law_firms, beneficiary,
    organized, biographical, arbitrage, national).

% Writes, remixes, or builds on existing expressive works without institutional legal support. Under this reading, incorporating any protected expression while creating new work is presumptively derivative-work preparation, exposing them to licensing demands or suits they cannot afford to litigate. Their practical exit is self-censorship: abandoning projects that touch existing expression at all, even where a narrower reading would clearly permit the use.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    powerless, biographical, trapped, global).

% Produces derivative fiction, art, and commentary built on existing fictional universes. This reading treats their transformative additions as irrelevant to the threshold question — any use of the underlying expression is enough to trigger derivative-work status — leaving the community dependent on rightsholder forbearance rather than legal right. Exit means abandoning the fandom's creative practice or migrating to jurisdictions with weaker enforcement, which fragments the community.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fan_transformative_communities, payer,
    powerless, biographical, constrained, global).

% Builds tools that ingest, transform, or reference copyrighted expression during training or generation. Under the enclosure reading, any incorporation of expressive content in the creative pipeline is presumptive derivative-work preparation, forcing costly pre-clearance licensing that only well-capitalized incumbents can absorb. Cannot afford the litigation risk of testing a narrower boundary in court, so bottlenecks its own development pipeline preemptively.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, small_ai_and_software_developers, payer,
    moderate, biographical, constrained, national).

% Uses clips, quotations, and excerpts of copyrighted works for commentary and criticism. Even though such uses are traditionally the strongest fair-use candidates, treating any expressive incorporation as derivative-work preparation shifts the practical burden onto them to obtain licenses preemptively rather than defend a use after the fact, chilling investigative and critical work that touches powerful rightsholders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, documentary_and_criticism_producers, payer,
    moderate, biographical, constrained, national).

% Preserves and provides access to cultural works, sometimes producing derivative formats (transcriptions, adaptations for accessibility) that incorporate original expression. Faces licensing demands or threatened suits for preservation activities that would be uncontroversial coordination under a narrower reading; must divert budget to clearance rather than preservation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, library_and_archive_institutions, payer,
    moderate, generational, constrained, national).

% Adjudicates the boundary of the derivative work right and could narrow or codify it differently. Case law and statutory reform are the mechanisms by which this reading could be displaced by a coordination or hybrid reading; some judicial opinions already resist the broadest reading advanced by rightsholders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, legacy_rightsholder_conglomerates).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its strongest form, the derivative-work right coordinates a real problem: it lets original authors capture some of the value of adaptations of their work (sequels, translations, adaptations) so that the incentive to create the underlying work is not undermined by uncompensated exploitation of its most valuable extensions.
% TRANSFER_FUNCTION: Under this reading, the boundary is drawn so broadly that it moves licensing revenue and settlement payments from anyone who incorporates any protected expression while creating new work — regardless of transformation — to incumbent rightsholders and the licensing/litigation infrastructure built around them.
% ABSENT_VOICES: Independent creators, fan communities, and small developers who cannot afford to litigate the boundary have no seat in the courts or legislative bodies that could narrow the reading; their preferred narrower standard (the coordination_reading) is argued mainly by amici and public-interest groups, not by the parties bearing the cost directly.
% DISAPPEARANCE_RATIONALE: If the enclosure reading vanished and courts uniformly adopted a narrower substantial-incorporation standard, the licensing-intermediary and litigation-specialist industries built around broad presumptive derivative status would shrink sharply, transformative and fan communities would operate without preemptive clearance, and rightsholder revenue from marginal-use licensing would fall — a large share of current settlement and licensing activity depends on the breadth of this specific reading, not on any narrower alternative.
% FOUNDING_PROBLEM: The derivative-work right was created to prevent uncompensated exploitation of an author's most commercially significant extensions of their own work (sequels, translations, adaptations) that would undermine the incentive to create in the first place.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder trade associations and licensing intermediaries attest the broad reading remains necessary to prevent erosion of the incentive structure. Independent legal scholars, public-interest copyright organizations, and several appellate opinions attest that the founding problem (protecting the market for authorized adaptations) is adequately served by a substantial-incorporation or transformative-use standard, and that the enclosure reading has drifted from incentive-protection into rent extraction from parties who pose no market substitution threat.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.82, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.82 at interval end) because the boundary as read here captures value with no requirement that the new work substitute for or harm the market of the original — the trigger is mere incorporation of expression, not substantial appropriation or commercial competition. Suppression is authored high (0.79) because persistence depends on maintaining the presumption pre-creation: the chilling effect operates before any use occurs, which is a stronger suppressive mechanism than post-hoc liability. Theater is low-moderate (0.28) because enforcement is not mostly performative — settlements and licensing fees are real transfers, though a growing share of enforcement activity targets uses with negligible market-substitution risk, which is where the performative/rent-seeking share is concentrated.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder/licensing-intermediary seat, the arrangement looks like straightforward property enforcement — every use of protected expression is exactly the kind of exploitation the exclusive right was designed to capture. From the independent-creator and fan-community seats, the identical structure operates as a snare: pre-creation licensing gates that must be satisfied regardless of transformation, with no meaningful path to contest the boundary given asymmetric litigation resources. The engine computes these as different seat-level classifications from the same structural data; the divergence is the intended measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Legacy rightsholders and licensing/litigation intermediaries are declared beneficiaries with arbitrage-grade exit — they can restructure licensing terms or select which claims to bring, and their revenue increases with the boundary's breadth, placing them near the full-beneficiary end of directionality. Independent creators and fan communities are declared victims with trapped or constrained exit — they cannot avoid using existing cultural expression as raw material for new expressive work, and litigation is financially foreclosed, placing them near the full-target end. Moderate-power payers (small developers, documentary producers, archives) sit closer to the target end than symmetric because their exit (preemptive licensing or abandoning projects) is costly but not fully blocked.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting an author's market for authorized adaptations — remains partially live (adaptations and sequels retain real commercial value), which is why founding_problem_status is authored as contested rather than dead. But the enclosure reading's breadth has drifted well past that founding problem: it now captures uses (fan transformation, criticism, preservation, tool-building) that pose no plausible threat to the adaptation market the right was built to protect. Classifying this as snare rather than tangled_rope reflects that the coordination story (protecting adaptation markets) is largely cover for the boundary's actual operation, which is extraction from parties who do not compete with the original market at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the statutory text of the derivative-work right actually compel the enclosure reading, or is the breadth a doctrinal accretion layered onto a narrower original standard by litigation and lobbying pressure over time?',
    'Comparative doctrinal history: trace how courts moved (or did not move) from a substantial-similarity/fixed-recasting standard toward a broader incorporation-triggers-liability standard, and identify whether legislative text changed or only judicial interpretation drifted.',
    'If the breadth is interpretive drift rather than textually compelled, the enclosure reading is more vulnerable to displacement by the coordination_reading through ordinary litigation, without requiring statutory reform — lowering the confidence that this reading''s high ε is stable long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the enclosure reading''s breadth is textually required or accreted through interpretation and enforcement pressure.').

omega_variable(
    market_substitution_test_absence,
    'Would explicitly requiring a market-substitution or commercial-harm showing before derivative-work liability attaches collapse most of the enclosure reading''s extractive reach, or would rightsholders retain most leverage through settlement pressure regardless of the legal standard?',
    'Empirical study of settlement outcomes in jurisdictions or time periods where courts have required stronger substitution showings, compared to settlement rates under the current broad standard.',
    'If settlement pressure persists regardless of legal standard (because litigation cost alone extracts value even from meritless claims), reclassifying the doctrinal standard would not by itself resolve the extraction — implicating the litigation cost structure itself as a separate constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_substitution_test_absence, empirical, 'Whether narrowing the legal standard would actually reduce extraction, or whether litigation-cost asymmetry independently sustains it.').

omega_variable(
    reading_selection_evidentiary_basis,
    'What specific case law, industry practice, or enforcement pattern justifies treating the enclosure reading (rather than the coordination or hybrid reading) as the operative standing arrangement for this story''s ε, given that all three readings claim the same statutory text?',
    'Systematic review of enforcement letters, licensing-demand practice, and litigation filing patterns across major rightsholder organizations to determine which reading actually governs day-to-day enforcement behavior, as distinct from which reading courts formally endorse.',
    'If enforcement behavior in practice tracks the coordination_reading more closely than formal doctrine suggests, this story''s authored ε may overstate the enclosure reading''s operative reach; if enforcement behavior is even broader than formal doctrine, ε may understate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Whether day-to-day enforcement practice matches the enclosure reading as strictly as the formal doctrinal claim suggests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(deri_tr_t6, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(deri_tr_t18, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(deri_be_t6, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(deri_be_t18, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(deri_su_t6, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(deri_su_t18, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the derivative_work_statutory_boundary kernel. coordination_reading authors a substantially lower ε (narrow, substantial-incorporation trigger; transformative/intermediate uses exempt). hybrid_carveout_reading authors an intermediate ε keyed to commercial exploitation (non-commercial transformative use permitted, commercial use requires authorization). This story (enclosure_reading) authors the highest ε: any expressive incorporation triggers derivative status regardless of transformation or commercial purpose. The three share the same statutory kernel and beneficiary/victim architecture in outline but diverge sharply on extraction magnitude, suppression mechanism, and classification (snare here vs. likely rope/tangled_rope in the siblings) because each reading draws the operative boundary at a different point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
