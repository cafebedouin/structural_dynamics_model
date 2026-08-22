% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Religious Authority Co-Constitution (Feedback Loop Reading)
 *   domain: history/technology/religious_institutional
 *
 * SUMMARY:
 *   Between 1440 and 1540, printing technology and religious reformation
 *   co-constituted each other through feedback loops. Printing entrepreneurs
 *   invested in religious texts because religious controversy generated
 *   profitable demand. Reformers adopted printing to scale their theological
 *   reach because the technology made mass distribution feasible. The Church
 *   responded with active suppression (indexing, book burning, prosecution)
 *   that paradoxically increased demand for prohibited texts. No single party
 *   controlled the dynamic; instead, economic incentives, technological
 *   capability, and institutional contestation created a self-reinforcing
 *   cycle. This reading rejects technological determinism (the press did not
 *   inevitably cause the Reformation) and strategic deployment (neither
 *   printers nor reformers fully orchestrated the outcome). Instead, human
 *   agency and technological affordance co-produced the Reformation's pace
 *   and distribution through structured but uncoordinated feedback loops.
 *
 * KEY AGENTS:
 *   - printing_entrepreneurs: organized actors seeking profitable markets; exit is mobile (can relocate or retool); benefit economically from religious controversy
 *   - reformation_reformers: moderate power, identity-locked to their theological project; benefit from printing reach but depend on printer discretion; constrained by market incentives
 *   - established_church_authority: institutional power, civilizational time horizon; bear costs of losing textual monopoly; constrained exit (cannot abandon institutional form); enforce suppression actively
 *   - manuscript_scribal_workforce: powerless, trapped; displaced by printing technology; absorb concentrated costs; have no say in adoption
 *   - ecclesiastical_authority_apparatus: agenda setters enforcing prohibition; create feedback loop (prohibition increases notoriety, drives demand, sustains profitable printing)
 *   - observer_historians: reconstruct the feedback loop structure and identify co-constitution rather than uni-directional causality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.58).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.52).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Religious Authority Co-Constitution (Feedback Loop Reading)").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history/technology/religious_institutional").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '73128aed-442f-4703-be4a-f925ccd513ed').
narrative_ontology:cs_kernel_codification('73128aed-442f-4703-be4a-f925ccd513ed', distributed).
narrative_ontology:cs_authority_grounding('73128aed-442f-4703-be4a-f925ccd513ed', distributed).
narrative_ontology:cs_reading_relation('73128aed-442f-4703-be4a-f925ccd513ed', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('73128aed-442f-4703-be4a-f925ccd513ed', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('73128aed-442f-4703-be4a-f925ccd513ed', foundational, mutual_constitution_not_determination).
narrative_ontology:cs_axiom_status(mutual_constitution_not_determination, holdable).
narrative_ontology:cs_axiom_grounding('73128aed-442f-4703-be4a-f925ccd513ed', mutual_constitution_not_determination, empirically_contingent).
narrative_ontology:cs_axiom('73128aed-442f-4703-be4a-f925ccd513ed', foundational, feedback_loop_as_primary_causal_structure).
narrative_ontology:cs_axiom_status(feedback_loop_as_primary_causal_structure, holdable).
narrative_ontology:cs_axiom_grounding('73128aed-442f-4703-be4a-f925ccd513ed', feedback_loop_as_primary_causal_structure, instrumental).
narrative_ontology:cs_reference_frame('73128aed-442f-4703-be4a-f925ccd513ed', printing_reformation_feedback_intact).
narrative_ontology:cs_drift_state('73128aed-442f-4703-be4a-f925ccd513ed', post_1540_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73128aed-442f-4703-be4a-f925ccd513ed', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printing_entrepreneurs).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformation_reformers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, established_church_authority).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, manuscript_scribal_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reading_audiences).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, reformation_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Printers establish printing operations in major cities and seek profitable markets. Religious controversy—particularly around Reformation texts—becomes a highly profitable niche: prohibited books, vernacular Bibles, polemical tracts sell. They benefit economically from the demand generated by religious conflict and invest in expanding capacity to serve it. Their exit is geographic: they can relocate to more profitable jurisdictions or pivot to other profitable genres, but once invested in the religious-text market, they are subject to censorship pressure and patron dependence.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printing_entrepreneurs, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printing_entrepreneurs, beneficiary).

% Reformers (Luther, Zwingli, Calvin, and their networks) seek to reach wider audiences with theological arguments and vernacular scripture. The printing press enables mass distribution of their writings in their own lifetimes—a capability medieval disputants never had. They benefit enormously from the technology's existence and the printers' willingness to publish them. They also pay: they depend on printer discretion and market demand, face intense persecution when caught, and must navigate the economic incentives of printers who sometimes dilute or distort texts for market appeal. Their exit is constrained by ideological commitment (identity_locked): abandoning the printing channel means abandoning the broader Reformation project itself.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_reformers, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reformation_reformers, payer).

% The papal hierarchy, bishops, and established ecclesiastical authority face the technology as a destabilizing force. Their authority relied on textual scarcity, clerical monopoly on interpretation, and slow circulation of challenging ideas. Printing accelerates the reach of criticism, amplifies dissent, and enables reformers to scale their message faster than institutional rebuttal can. The Church's response is prohibition and censorship (active enforcement), but the technology's economic profitability means printers continue producing banned texts where there is demand. The Church cannot exit without fundamentally abandoning its institutional form.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, established_church_authority, payer,
    institutional, civilizational, constrained, universal).

% Scribes who copied manuscripts by hand lose their primary source of income as printed books become cheaper and faster to produce. Many transition to other work (binding, proofreading, other trades), but the displacement is real and concentrated. They have no say in the technological adoption and no alternative labor market that values their particular skill.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, manuscript_scribal_workforce, payer,
    powerless, biographical, trapped, local).

% Literacy rates increase throughout the period. Audiences benefit from access to texts previously unavailable to them: vernacular Bibles, Reformation tracts, classical texts, and practical knowledge. The printing press makes these economically viable to produce and distribute. They are not organized actors in the constraint's operation but are the demand side that makes the constraint's dynamics possible.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reading_audiences, beneficiary,
    organized, generational, mobile, regional).

% Bishops, papal legates, and inquisitorial officials enforce prohibition of Protestant and heterodox texts. They establish indices of forbidden books, conduct book burnings, and prosecute printers and distributors. This active enforcement sustains the extraction of control and maintains suppression of alternatives—but it cannot prevent the technology from functioning. The enforcement itself becomes a feedback loop: prohibition increases notoriety of texts, driving demand and making printing more profitable.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, ecclesiastical_authority_apparatus, agenda_setter,
    institutional, civilizational, constrained, universal).

% Civil authorities (princes, city councils) navigate pressure from both Church and reformers. Some permit printing of religious texts; others enforce censorship alongside ecclesiastical authorities. They are structurally excluded from authoring the fundamental constraint (the co-constitutive dynamic) because their position is reactive: they manage the fallout of the technology-religion interaction rather than determining its operation. Their role would be inclusion in a different story about political contestation over printing.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, civil_magistrates, excluded,
    organized, generational, constrained, regional).

% Historical analysis of the period reveals the feedback loop: printing enabled Reformation; Reformation demand created printing markets; markets drove technological investment; technological scaling enabled wider Reformation reach; institutional backlash stimulated demand for prohibited books. The observer seat reconstructs the causal chain and identifies the constraint as co-constitutional rather than uni-directional.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, observer_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing technology solves a coordination problem for distributed reformers: how to reach geographically dispersed audiences with consistent theological argument and vernacular scripture when no prior mechanism existed. It simultaneously solves a distribution problem for printers: how to profitably scale text production. The coordination is not between willing parties but emerges from overlapping incentives (reformer need for reach, printer need for profitable markets, audience demand for texts).
% TRANSFER_FUNCTION: The arrangement transfers economic value and institutional authority. Printers extract monopoly rents from religious controversy (printed materials command premium prices in suppressed markets). Reformers extract reach and audience engagement they could not achieve otherwise. Established Church authority is extracted from—their control over textual circulation and interpretation erodes. Scribal labor is extracted from—displaced by cheaper printing. The transfers flow in multiple directions: no single direction characterizes the constraint.
% ABSENT_VOICES: Civil magistrates are structurally excluded from authoring the core dynamic (though they respond to it). Women, peasants, and non-literate populations are absent from the constraint's operation despite being targets of Reformation preaching; they consume the outputs but do not set the terms. Printers' wives and family members who often did binding and distribution work are invisible in formal accounts. These absences shape the distribution of who benefits and who bears costs.
% DISAPPEARANCE_RATIONALE: If the printing technology and the institutional/economic arrangements supporting it vanished overnight, the Reformation would have proceeded much more slowly and with different regional outcomes. Reformation ideas would still circulate but would be limited to oral networks, manuscript copying, and face-to-face teaching—the pre-print mechanisms. Established Church authority would retain stronger control over information flow. The religious diversity that emerged in Europe was enabled by printing's scale; without it, the Reformation becomes a longer, more geographically fragmented process or stalls in regions where oral transmission is less effective. The world clearly rearranges, but not because printing is absolutely necessary to Reformation theology—rather because the feedback loop between technology and religious controversy accelerated and distributed the process in ways that became irreversible once launched.
% FOUNDING_PROBLEM: Reformers in the 15th century faced the problem of reaching audiences beyond their immediate circles with theological arguments and corrected scripture. The Church monopolized scriptural authority through clerical training and textual scarcity. Simultaneously, printers faced the problem of finding profitable markets for newly invented printing technology. The constraint emerges from the collision of these two problems: printing technology provided the enabling infrastructure, but only the economic incentive structure created by religious controversy made large-scale printing of religious texts viable.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the book trade (Andrew Pettegree, Elizabeth Eisenstein, Adrian Johns) attest that religious texts were among the highest-demand printed materials and that printing entrepreneurs pursued this market aggressively. Reformers' own writings and correspondence (Luther's letters, Calvin's dedications) attest their sense that printing was enabling their reach in unprecedented ways. The Church's own prohibition efforts and index creation attest to the threat printing posed to their information control. Independent sources (printer colophons, bookseller records, censorship documents) corroborate that the feedback loop was actively operating.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1440: printing exists but religious controversy is not yet the dominant market) to 0.58 (1525: peak of Reformation ferment, maximal extraction from religious texts; plateau by 1540 as Protestant territories stabilize and markets mature). Suppression follows a similar trajectory: begins low (no organized Church response when threat is not yet clear) and peaks around 1510–1525 (Index of Prohibited Books, aggressive prosecution, book burnings). Theater ratio rises steadily as the Church's enforcement becomes increasingly performative—dramatic public burnings and index publications are theatrical displays aimed at moral authority maintenance, not effective suppression (prohibited books continue circulating and selling). The measurement grid is aligned across all three metrics: every time point has all three metrics authored. The co-constitution reading centers the feedback loop itself: technology enables market demand, market demand incentivizes technology investment, institutional suppression increases demand for prohibited materials, increased demand sustains the cycle. No single actor orchestrates this; all are caught in the amplifying mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the printing_entrepreneurs' seat, the constraint looks like a rope (genuine coordination of supply and demand for profitable texts; the technology solves a real distribution problem). From the reformation_reformers' seat, it looks like a scaffold (temporary enabling infrastructure they depend on for a specific historical moment; once Protestant territories establish, the printing economy normalizes and the constraint's function shifts). From the established_church_authority seat, it looks like a snare (they are trapped; suppression requires constant investment but cannot stop the technology; exit is impossible). From the manuscript_scribal_workforce seat, it is pure extraction with no coordination benefit (they are displaced with no alternative). The engine computes per-seat divergence from the structural data; the claiming seat (co-constitution) asserts the overall dynamic is mutual constitution, not extraction-only or determination-only.
 *
 * DIRECTIONALITY LOGIC:
 *   printing_entrepreneurs: d ≈ 0.2 (beneficiaries with mobile exit; they can leave the religious-text market if it becomes unprofitable or dangerous). reformation_reformers: d ≈ 0.4 (genuine coordination benefit from the technology, but identity-locked exit and dependence on printer discretion pull d higher; they benefit but at risk). established_church_authority: d ≈ 0.85 (targets; they bear costs of lost authority; constrained exit). manuscript_scribal_workforce: d ≈ 0.9 (targets; concentrated displacement; trapped exit). No override needed: the derivation from beneficiary/victim + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reach dispersed audiences with theological arguments) was genuinely live in 1440–1525. By 1540, in Protestant territories, printing has become normalized infrastructure; the founding problem is solved and the constraint's function shifts. In Catholic territories, the founding problem remains live because suppression continues and prohibited texts remain scarce. The mandatrophy status is CONTESTED: the reading must distinguish between Protestant and Catholic territories' different classifications. In the Protestant case, the constraint shows mandatrophy symptoms (the founding function is solved; what persists is the printing economy itself, not the Reformation-enabling dynamic). In the Catholic case, suppression remains active and the Reformation remains contestatory, so mandatrophy has not set in. This story's interval endpoint (1540) falls at the boundary between these trajectories. The constraint is still live overall, but the regional divergence is the key finding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermined,
    'Is the relationship between printing and Reformation best understood as co-constitution, as technological enabling, or as strategic deployment—and do these framings describe fundamentally different constraints or interpretations of one constraint?',
    'Comparative analysis of the three readings as constraints: if they have structurally different ε values and different victim sets, they are distinct constraints (ε-invariance principle). If they have the same structural properties but different narratives, they are interpretations of one constraint.',
    'If the readings are distinct constraints (different ε, different beneficiaries/victims per seat), then each should be authored as a separate story linked via network.affects_constraints. If they are interpretations of one constraint, the committer frame captures the underdetermination via omega variables rather than constraint multiplication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermined, conceptual, 'Whether co-constitution, technological determinism, and strategic deployment are distinct constraints or interpretations of one kernel.').

omega_variable(
    feedback_loop_or_coincidence,
    'Did the feedback loop between printing and Reformation actively reinforce itself, or do the historical correlations reflect coincidental alignment of two independent trends (printing''s technological progress and Reformation''s theological innovation)?',
    'Counterfactual analysis: simulation or historical comparison of what printing''s adoption would look like without Reformation demand, and what Reformation spread would look like without printing. Analysis of printer business records: did religious texts actually generate higher margins than other genres? Did printers shift investment toward religious texts in response to Reformation controversy?',
    'If the loop is real (feedback loop confirmation), the constraint is tangled_rope with genuine co-constitution. If the trends are coincidental, the constraint becomes scaffold (printing is temporary infrastructure) or rope (coordination without extraction). A weak feedback loop would lower extractiveness and revise the type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_or_coincidence, empirical, 'Whether the measured feedback loop is a causal mechanism or correlation artifact.').

omega_variable(
    suppression_internalization,
    'To what extent did the Church''s suppression of religious texts operate as structural coercion (legal barriers, physical threats) versus internalized acceptance of censorship (readers and printers internalizing the Church''s authority claims about what texts are legitimate)?',
    'Historical records of book smuggling, clandestine printing, and readers'' willingness to seek prohibited texts despite legal risk. Post-Reformation archival analysis: did suppression become less necessary as religious diversity institutionalized?',
    'If suppression is primarily structural, it is a measurable enforcement cost. If internalized, the measured suppression underestimates the constraint''s effective strength because targets carry suppression with them even absent external barriers. This affects classification: high internalization suggests snare (targets are psychologically trapped); low internalization suggests the constraint relies on active, costly enforcement (sustains tangled_rope characterization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression in the Church''s textual prohibition regime.').

omega_variable(
    market_inevitability,
    'Given printing technology, would some market inevitably emerge for religious texts, or was the specifically Reformation-driven market a historical contingency dependent on theological innovation?',
    'Historical analysis of printing in regions where Reformation never took hold (parts of Italy, Spain): did printers find alternative profitable markets, or did the lack of theological demand constrain printing''s growth? Technological history: what was printing used for before the Reformation (incunabula focus)?',
    'If printing-market emergence was inevitable given the technology, technological determinism gains credibility and co-constitution''s emphasis on feedback is overdrawn. If the Reformation-specific demand was crucial to printing''s profitability and expansion, co-constitution is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_inevitability, empirical, 'Whether printing''s economic viability was inevitable or contingent on Reformation-driven demand.').

omega_variable(
    co_constitution_vs_determinism_underdetermined,
    'What is the minimum evidence required to distinguish co-constitution (mutual feedback) from technological determinism (press enables, reformation responds) in the historical record?',
    'Detailed temporal analysis: did printing investment precede Reformation demand, or did Reformation demand precede printing adoption in specific regions? Do regions of rapid Reformation adoption correlate with regions of intense printing investment? Does the correlation go both directions or primarily one direction?',
    'Unidirectional causality (printing → Reformation adoption) supports technological determinism and scales down co-constitution''s claim. Bidirectional temporal overlap with mutual reinforcement supports co-constitution. Timeline analysis could split this constraint into two: one for the enabling phase (press → literacy → receptivity) and one for the feedback phase (Reformation demand → printing investment → reach → reformation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_constitution_vs_determinism_underdetermined, empirical, 'Distinguishing co-constitutive feedback from technological causality via temporal analysis.').

omega_variable(
    regional_variance_under_determination,
    'Does the co-constitution mechanism operate uniformly across Protestant and Catholic regions, or do the two regions instantiate structurally different constraints?',
    'Regional comparative analysis: Catholic territories where Reformation was suppressed but printing was allowed (e.g., parts of Italy): what did printers print? Protestant territories where Reformation succeeded: how did printing adoption rates and investment patterns compare before vs. after Reformation establishment? Do the regions show different extraction patterns, different beneficiaries?',
    'If regional variance is substantial, the constraint should decompose into two stories: co-constitution (Protestant territories, 1440–1540) and suppression-with-printing (Catholic territories, same interval). This affects the single-kernel vs. multiple-kernels characterization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_variance_under_determination, empirical, 'Whether co-constitution operates uniformly or splits into region-specific constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1440, 1540).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causality__co_constitution, theater_ratio, 1440, 0.08).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causality__co_constitution, theater_ratio, 1470, 0.14).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causality__co_constitution, theater_ratio, 1490, 0.24).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causality__co_constitution, theater_ratio, 1510, 0.34).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__co_constitution, theater_ratio, 1525, 0.4).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__co_constitution, theater_ratio, 1540, 0.41).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causality__co_constitution, base_extractiveness, 1440, 0.15).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causality__co_constitution, base_extractiveness, 1470, 0.28).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causality__co_constitution, base_extractiveness, 1490, 0.42).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causality__co_constitution, base_extractiveness, 1510, 0.51).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__co_constitution, base_extractiveness, 1525, 0.58).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__co_constitution, base_extractiveness, 1540, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causality__co_constitution, suppression_requirement, 1440, 0.1).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causality__co_constitution, suppression_requirement, 1470, 0.22).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causality__co_constitution, suppression_requirement, 1490, 0.38).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causality__co_constitution, suppression_requirement, 1510, 0.48).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__co_constitution, suppression_requirement, 1525, 0.54).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__co_constitution, suppression_requirement, 1540, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.18).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, manuscript_displacement_labor_market).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, church_information_monopoly_erosion).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the press_reformation_causality kernel. The co-constitution reading asserts mutual feedback between printing economy and religious controversy, rejecting both technological determinism (press as autonomous cause) and strategic deployment (intention-driven weaponization). Sibling constraints instantiate the alternative readings; this story links to them via affects_constraints to show family relationship. Decomposition rationale (ε-invariance): the three readings have structurally different ε values and victim sets, justifying separate constraint stories rather than a single constraint with measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
