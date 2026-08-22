% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press Made the Reformation Inevitable (Technological-Determinist Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological-determinist reading of the
 *   technology_reformation_causality kernel: the claim that movable-type
 *   printing, by collapsing the marginal cost of text reproduction, made mass
 *   vernacular scripture distribution irresistible and the Reformation
 *   inevitable, with reformers as downstream adapters of a force they did not
 *   direct. The claim/metric gap is deliberate and is the datum: the reading
 *   CLAIMS mountain (the physics of reproduction cost presented as a natural
 *   law of history) while the authored metrics describe a contested, actively
 *   defended historiographical arrangement with identifiable beneficiaries,
 *   which is false-summit territory. Decomposition note: the colloquial label
 *   printing-press-caused-the-Reformation conflates two structurally distinct
 *   claims. The physical cost-collapse fact (authored separately as
 *   print_reproduction_cost_floor, a genuine mountain with negligible
 *   extraction) is upstream evidence this reading cites; THIS story authors
 *   the inevitability claim itself, whose epsilon differs from the cost
 *   fact's by a wide margin because the inevitability version is contested,
 *   counterexample-laden, and paradigm-protecting. KEY AGENTS (by structural
 *   relationship): media_ecology_theorists: primary beneficiary
 *   (institutional/identity_locked) — collects paradigm authority;
 *   print_history_specialists: secondary beneficiary (organized/constrained);
 *   secularization_narrative_historians: beneficiary
 *   (institutional/constrained); survey_textbook_publishers: agenda setter
 *   (institutional/arbitrage) — administers the claim's circulation without
 *   convictional commitment; reformation_agency_historians: primary target
 *   (organized/constrained) — bears foreclosure of explanatory space;
 *   doctrinal_causation_scholars: target (moderate/constrained);
 *   non_european_print_histories: target (powerless/trapped) —
 *   counterexamples absorbed as exceptions; reformation_era_protagonists:
 *   excluded voice (powerless/trapped); general_audience_history_consumers:
 *   incidental beneficiary and payer (moderate/mobile);
 *   historiographical_methodologists: analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.58).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.57).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press Made the Reformation Inevitable (Technological-Determinist Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__technological_determinism_reading).
domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '2f587de1-5a22-44ee-9978-6d513a955dc2').
narrative_ontology:cs_kernel_codification('2f587de1-5a22-44ee-9978-6d513a955dc2', distributed).
narrative_ontology:cs_authority_grounding('2f587de1-5a22-44ee-9978-6d513a955dc2', lineage).
narrative_ontology:cs_interpretation_layer_present('2f587de1-5a22-44ee-9978-6d513a955dc2').
narrative_ontology:cs_reading_relation('2f587de1-5a22-44ee-9978-6d513a955dc2', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('2f587de1-5a22-44ee-9978-6d513a955dc2', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('2f587de1-5a22-44ee-9978-6d513a955dc2', foundational, technological_infrastructure_determines_religious_outcomes).
narrative_ontology:cs_axiom_status(technological_infrastructure_determines_religious_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('2f587de1-5a22-44ee-9978-6d513a955dc2', technological_infrastructure_determines_religious_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('2f587de1-5a22-44ee-9978-6d513a955dc2', secondary, agent_choice_epiphenomenal_under_cost_collapse).
narrative_ontology:cs_axiom_status(agent_choice_epiphenomenal_under_cost_collapse, holdable).
narrative_ontology:cs_axiom_grounding('2f587de1-5a22-44ee-9978-6d513a955dc2', agent_choice_epiphenomenal_under_cost_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('2f587de1-5a22-44ee-9978-6d513a955dc2', press_as_sufficient_causal_engine).
narrative_ontology:cs_drift_state('2f587de1-5a22-44ee-9978-6d513a955dc2', contemporary_post_agency_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f587de1-5a22-44ee-9978-6d513a955dc2', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, media_ecology_theorists).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, print_history_specialists).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, secularization_narrative_historians).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, reformation_agency_historians).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, doctrinal_causation_scholars).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, non_european_print_histories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, general_audience_history_consumers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, general_audience_history_consumers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, technological_determinism_doctrine).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, mcluhan_gutenberg_injunction).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, eisenstein_print_revolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and transmit the McLuhan-lineage framework in which media forms shape consciousness and history; the Reformation case is their discipline's anchor demonstration. Chairs, journals, and graduate programs reproduce the claim, and their own training is constituted by it. Leaving the claim would not just discard a result, it would unsettle the field's founding gesture.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, media_ecology_theorists, beneficiary,
    institutional, generational, identity_locked, global).

% Build careers on the archive of print shops, editions, and diffusion data; the claim directs funding, attention, and curricular space toward their materials. Their empirical work would survive a weakening of the strong causal version, but the field's public rationale is tied to print's centrality in the story of modernity.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, print_history_specialists, beneficiary,
    organized, biographical, constrained, global).

% Fold the claim into modernization narratives in which technology drives religious transformation and decline; it supplies a ready-made mechanism step between Gutenberg and the secular age. Their broader frameworks could survive without it, but replacing it would require rebuilding a hard explanatory link.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, secularization_narrative_historians, beneficiary,
    institutional, generational, constrained, continental).

% Decide which causal story enters standard survey texts and revise on commercial revision cycles. They circulate and thereby maintain the claim without deep commitment to it; if the teaching market shifted to a different causal frame, they would print that instead. Their enforcement is administrative, not convictional.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, survey_textbook_publishers, agenda_setter,
    institutional, immediate, arbitrage, global).

% Study Luther's choices, printer-publisher financing alliances, imperial politics, and city-council decisions. To be heard in general venues they must first engage the technological frame on its own terms; their positive accounts appear in specialty outlets while survey narratives remain unchanged. Their rebuttal labor is ongoing and largely uncompensated in reputational terms.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformation_agency_historians, payer,
    organized, biographical, constrained, global).

% Attribute the Reformation to theological developments: justification by faith, the indulgence controversy, liturgical and pastoral change. Under the prevailing causal story their entire explanatory register is subordinated to distribution mechanics, with doctrine treated as interchangeable content that any sufficiently cheap medium would have carried.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, doctrinal_causation_scholars, payer,
    moderate, biographical, constrained, national).

% Document movable-type printing in Korea in the thirteenth century and in China centuries before Luther, and print's non-revolutionary integration in many societies. Their cases enter mainstream accounts only as footnoted exceptions that reportedly do not count, and there is no exit from that anomalous status: the stronger their evidence, the more it is filed as anomaly rather than as disconfirmation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, non_european_print_histories, payer,
    powerless, biographical, trapped, global).

% The reformers, printers, and church authorities whose recorded testimony about their own motives, providence, vocation, profit, fear, is preserved in the archive but not admitted as causal evidence. The reading casts them as downstream adapters of forces they did not comprehend, denying authorship of the movement to the people who made it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformation_era_protagonists, excluded,
    powerless, civilizational, trapped, continental).

% Receive the tidy one-line causal story through documentaries, popular books, and survey courses. They gain an easily retold narrative and pay for it with a flattened picture in which human decision, contingency, and doctrine drop out of how the modern world began.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, general_audience_history_consumers, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, general_audience_history_consumers, payer).

% Assess causal standards in historical explanation: counterfactual testability, overdetermination, the difference between enabling conditions and causes. They evaluate the inevitability claim without holding stakes in any of the competing readings, and note that no run of history without print exists to test it against.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, historiographical_methodologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__technological_determinism_reading, media_ecology_theorists).
narrative_ontology:fixing_cost_class(technology_reformation_causality__technological_determinism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared causal spine for teaching and discussing the Reformation: one sentence that organizes survey courses, documentary scripts, and cross-disciplinary conversation, letting nonspecialists exchange a common reference point without engaging four centuries of scholarship.
% TRANSFER_FUNCTION: Moves explanatory authority, curricular share, and citation capital from agency-centered and doctrinal accounts toward technology-centered ones; the transfer is paid for by the subordination of rival explanations and by the recasting of historical agents as passive recipients.
% ABSENT_VOICES: Reformation-era protagonists' own testimony about motive and cause is excluded, as the reading denies authorship to the very agents it describes. Non-European print historians participate only as footnoted anomalies. Both would object that the causal account was assembled without its subjects and against its strongest counterexamples.
% DISAPPEARANCE_RATIONALE: Survey curricula, media-theory syllabi, and popular history would lose their organizing sentence overnight; the agency and co-constitution readings would move from rebuttal position to default; media ecology would lose its anchor case. Books and churches themselves would not change, because the rearrangement is in historiographical arrangements, which is where this constraint operates.
% FOUNDING_PROBLEM: Explaining why Luther's protest survived repression when Hussitism and Wycliffism had not: early-sixteenth-century observers watched dissent crushed repeatedly, and the speed of printed replication exceeding censorship capacity looked like the missing variable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: agency historians and doctrinal scholars, the claim's principal critics, concede the original survival puzzle was real; counterfactual methodologists attest that the strong inevitability version outruns the available evidence; Korean and Chinese print historiography attests that press presence alone produces no reformation. No corroborating source outside the beneficiaries attests the inevitability version itself.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 at interval end: the claim gates explanatory space, curricular share, and citation flow, and it does so structurally rather than through overt sanction, which caps it below snare-range. Suppression 0.57: enforcement runs through peer-review gatekeeping, textbook canonization, and the framing of agency accounts as pre-analytic; a growing share is now held by inertia rather than conviction, and part is internalized, graduate training installs the frame as common sense, so suppression persists where no active enforcement exists (see omega internalized_paradigm_suppression). Theater 0.55 and rising: the word inevitable increasingly does rhetorical work the evidence cannot support, since inevitability as stated is unfalsifiable; specialist literature has largely migrated to nuanced multi-causal accounts while survey and popular invocations run on boilerplate. Accessibility collapse 0.30: alternatives remain fully workable, indeed two sibling readings of the same kernel are live positions, which honestly fails the mountain natural-law profile. Resistance 0.62: forty years of agency-turn critique, the Adams-Barker methodological challenge, and the global book-history turn constitute sustained organized opposition. The temporal series deliberately feed the extraction-accumulation trigger: base extractiveness climbed as the enforcement infrastructure matured through the 1990s canonization phase, then eased slightly after 2017 as a loose coalition of agency historians and non-European print historians gained ground, while survey teaching lagged scholarship. Metrics are authored independently of the mountain claim; the divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same structure. From the media-ecology seat the claim is settled science anchoring a discipline, its enforcement invisible because it coincides with consensus; from the agency-historian seat the same claim is a foreclosure that must be engaged before any positive account can be heard; from the textbook-publisher seat it is inventory, enforced administratively and abandoned instantly if the market moves; from the non-European print-history seat it is a trap in which stronger counterexamples produce stronger anomaly-classification rather than revision. Identity-lock dynamics concentrate on the media-ecology seat: the lock is ideological-professional fusion, the medium-is-the-message injunction is constitutive of the field's self-understanding, so if the frame broke the field would not lose a claim, it would lose its founding gesture, and the classification of that seat would shift sharply toward mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: media_ecology_theorists collect the doctrine-specific premium directly (d nearest zero, amplified by identity_locked exit); print_history_specialists collect attention and funding spillover with constrained but real exit; secularization_narrative_historians collect a saved explanatory burden with the most mobility of the three. Targets sit near the full-target end: agency historians bear foreclosure with constrained exit, doctrinal scholars bear subordination of their entire register, and non-European print historians bear the anomalous-absorption dynamic with no exit at all, the highest-directionality seat in the story. General-audience consumers sit near symmetric: genuine benefit from a teachable story, genuine cost in a flattened picture. Survey_textbook_publishers derive near-beneficiary position but their arbitrage exit damps any lock-in benefit. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation. Coalition note: the victims span organized, moderate, and powerless seats, and the analytically interesting fact is that a partial coalition has already formed, the global book-history turn linking agency historians with non-European print historians, which is visible in the post-2017 easing of the extractiveness series.
 *
 * MANDATROPHY ANALYSIS:
 *   Accepting the claim at its own presentation, as pure mountain, would erase the historiographical contest from the record and let paradigm-protection pass as natural law, which is precisely the false-summit failure this story is authored to expose: the beneficiaries are declared, the omega documenting the natural-law-versus-constructed ambiguity is mandatory, and the FSM signature evaluates whether the mountain claim survives contact with its own beneficiary structure. Reading it instead as pure extraction would err in the other direction: the claim does solve a real coordination problem, a shared causal spine for teaching, and it rests on genuine empirical content, the archival demonstration that print economics changed what was sayable at scale. The mandatrophy question is sharpened by the founding-problem interview: the narrow puzzle the claim was built to answer, why Luther survived when Hus did not, is regarded by specialists as substantially answered by multi-causal work, while the strong inevitability version persists in survey infrastructure, a mismatch the R5 consumer flags. The rising theater ratio tracks the same drift: functional analysis migrating out, ritual invocation remaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the press-constraint a genuine natural law of history, in which reproduction-cost physics made the Reformation inevitable regardless of any actor, or a constructed historiographical constraint whose persistence serves identifiable schools?',
    'Comparative counterfactual analysis across independent print adoptions: if print reliably produces religious rupture across cases varying in politics, censorship capacity, and doctrine (Korea, China, the Ottoman print lag, Catholic Counter-Reformation print use), the mountain reading strengthens; if outcomes vary systematically with agency and institutions, the constraint is constructed and the false-summit override applies.',
    'Genuine natural law would certify the mountain claim and dissolve the beneficiary structure as incidental; a constructed constraint routes through the false-summit signature toward a hybrid coordination-extraction classification with the named seats intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, empirical, 'Whether the inevitability claim is physics or paradigm protection.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the technology_reformation_causality kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Read the sibling files directly: beneficiary_agency_reading converts printers and reformers into agenda-setters and the Church into the paying seat, dissolving this file''s victim structure; co_constitution_reading removes the unilateral determination premise, leaving no seat with full-target directionality. The disagreement is located on a single axis: whether press-era outcomes were invariant under agent variation.',
    'Switching readings changes the epsilon referent, the beneficiary/victim sets, and the computed classification wholesale; no metric authored here transfers across readings, which is why the readings are separate files rather than parameters of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    counterfactual_testability_of_inevitability,
    'Is inevitability an empirical claim or a rhetorical device, given that no run of history without print exists to test it against?',
    'Structured counterfactual methodology: compare dissent-survival rates across polities before and after print adoption, controlling for censorship capacity and doctrinal appeal; if the claim cannot be given any discriminating test, it functions rhetorically.',
    'If rhetorical, the theater ratio understates the problem and the claim trends toward theatrical maintenance of a spent function; if empirical, it stands or falls on the comparative record and the classification follows the evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_testability_of_inevitability, conceptual, 'Whether the inevitability predicate is falsifiable or performative.').

omega_variable(
    epsilon_referent_governed_population,
    'Whose arrangement does epsilon measure here: the sixteenth-century arrangement the claim describes, or the historiographical arrangement the claim governs?',
    'Fix the governed population explicitly: the claim binds working historians, curriculum committees, textbook editors, and students, all living; the sixteenth-century participants are data, not governed parties. Authored epsilon assumes the historiographical referent.',
    'Under the historiographical referent the authored 0.58 stands; under a misread historical referent epsilon would approach zero and the story would misclassify as inert, so the referent choice is load-bearing for every downstream computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_governed_population, conceptual, 'Fixing what the extractiveness score is about.').

omega_variable(
    internalized_paradigm_suppression,
    'How much of the measured suppression is structural (gatekeeping, curricula, review) versus internalized (graduate training installing the frame as common sense)?',
    'Post-enforcement trajectory: examine whether scholars trained after the agency-turn critiques, in environments where active gatekeeping has relaxed, nonetheless reproduce the inevitability frame where no enforcement pressure exists; persistence under zero enforcement indicates internalized carryover.',
    'If substantially internalized, effective suppression exceeds the structural measure and would persist even after the beneficiary seats lost their enforcement capacity, raising the cost of any correction beyond what removing the gatekeepers would achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_paradigm_suppression, empirical, 'Structural versus internalized share of the suppression holding the claim in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1962, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1962, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement_basis(tech_tr_t1962, observed).
narrative_ontology:measurement(tech_tr_t1979, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1979, 0.22).
narrative_ontology:measurement_basis(tech_tr_t1979, observed).
narrative_ontology:measurement(tech_tr_t1993, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement_basis(tech_tr_t1993, observed).
narrative_ontology:measurement(tech_tr_t2005, technology_reformation_causality__technological_determinism_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement_basis(tech_tr_t2005, observed).
narrative_ontology:measurement(tech_tr_t2017, technology_reformation_causality__technological_determinism_reading, theater_ratio, 2017, 0.52).
narrative_ontology:measurement_basis(tech_tr_t2017, observed).
narrative_ontology:measurement(tech_tr_t2026, technology_reformation_causality__technological_determinism_reading, theater_ratio, 2026, 0.55).
narrative_ontology:measurement_basis(tech_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1962, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement_basis(tech_be_t1962, observed).
narrative_ontology:measurement(tech_be_t1979, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1979, 0.38).
narrative_ontology:measurement_basis(tech_be_t1979, observed).
narrative_ontology:measurement(tech_be_t1993, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement_basis(tech_be_t1993, observed).
narrative_ontology:measurement(tech_be_t2005, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement_basis(tech_be_t2005, observed).
narrative_ontology:measurement(tech_be_t2017, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement_basis(tech_be_t2017, observed).
narrative_ontology:measurement(tech_be_t2026, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(tech_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1962, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement_basis(tech_su_t1962, observed).
narrative_ontology:measurement(tech_su_t1979, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1979, 0.35).
narrative_ontology:measurement_basis(tech_su_t1979, observed).
narrative_ontology:measurement(tech_su_t1993, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement_basis(tech_su_t1993, observed).
narrative_ontology:measurement(tech_su_t2005, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement_basis(tech_su_t2005, observed).
narrative_ontology:measurement(tech_su_t2017, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement_basis(tech_su_t2017, observed).
narrative_ontology:measurement(tech_su_t2026, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 2026, 0.57).
narrative_ontology:measurement_basis(tech_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, identity_coordination).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, co_constitution_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, print_reproduction_cost_floor).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label printing-press-caused-the-Reformation covers two structurally distinct claims with widely different epsilon. print_reproduction_cost_floor (upstream, not authored in this file) is the physical-economic fact that movable type and paper collapsed reproduction costs, verified and uncontested, a genuine mountain with negligible extraction. This file authors the inevitability claim as instantiated by the technological-determinist reading: contested, counterexample-laden (Korean and Chinese precedents, pre-Luther presses, successful Catholic use of print), and paradigm-protecting, hence substantially extractive. The upstream cost fact is routinely cited as evidence for the downstream inevitability claim, which is why the family edge runs upstream-to-downstream. Kernel siblings beneficiary_agency_reading and co_constitution_reading instantiate the same causal question under different determination premises and carry their own beneficiary/victim structures; they are separate files, not hedges inside this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
