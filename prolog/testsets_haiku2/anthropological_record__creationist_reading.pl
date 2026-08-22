% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading of Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The creationist reading of the anthropological record is an institutional
 *   constraint that organizes religious communities' interpretation of human
 *   origins around divine agency and scriptural compatibility. This
 *   constraint is one reading of a contested kernel—the anthropological
 *   record itself—in which multiple epistemic communities (creationist
 *   religious traditions, naturalist evolutionary science, and indigenous
 *   knowledge systems) advance competing interpretations of the same
 *   empirical facts. The creationist reading sustains an institutional
 *   authority structure that benefits religious leadership and interpretation
 *   traditions while imposing costs on secular science, public education, and
 *   funding allocation. The constraint's persistence depends on active
 *   enforcement: religious institutions must continually assert creationist
 *   interpretations against materialist scientific claims, and they must
 *   defend that assertion in schools, legislatures, and public discourse. The
 *   claimed type is tangled_rope because the reading coordinates believers
 *   into a coherent faith-based interpretive tradition (genuine coordination
 *   function) while simultaneously extracting authority from secular science
 *   and education (asymmetric extraction from the payer seats). Neither the
 *   coordination nor the extraction function alone fully captures the
 *   structure; both are necessary.
 *
 * KEY AGENTS:
 *   - Religious institutional authority: sets the interpretive standard, defines what the record means within faith communities, collects legitimacy as the authoritative voice on origins
 *   - Creationist interpretation tradition: doctrine system that flourishes when scriptural sources are treated as epistemically valid
 *   - Secular scientific practitioners: must defend the legitimacy of evolutionary frameworks against organized epistemic challenge; bear enforcement costs
 *   - Public school biology curricula: subject to curricular contestation and pressure to include alternative explanations or dilute evolutionary teaching
 *   - Evolutionary research funding: faces legislative and public pressure to justify its epistemic legitimacy
 *   - Faith community members: benefit from an interpretation framework that does not require choosing between faith and understanding origins, but are identity-locked into that framework
 *   - Scientific credentialing bodies: enforce the requirement that credentialed natural science accept evolutionary frameworks; sustain the materialist epistemic monopoly within credentialed science
 *   - Excluded indigenous epistemologies: the absent voice whose own origin accounts and knowledge systems are systematically excluded from the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.71).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '5c368690-ab9e-43f5-9daa-4640053d5500').
narrative_ontology:cs_kernel_codification('5c368690-ab9e-43f5-9daa-4640053d5500', formalized).
narrative_ontology:cs_authority_grounding('5c368690-ab9e-43f5-9daa-4640053d5500', extraction).
narrative_ontology:cs_interpretation_layer_present('5c368690-ab9e-43f5-9daa-4640053d5500').
narrative_ontology:cs_reading_relation('5c368690-ab9e-43f5-9daa-4640053d5500', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('5c368690-ab9e-43f5-9daa-4640053d5500', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('5c368690-ab9e-43f5-9daa-4640053d5500', foundational, divine_agency_explanatory_necessity).
narrative_ontology:cs_axiom_status(divine_agency_explanatory_necessity, holdable).
narrative_ontology:cs_axiom_grounding('5c368690-ab9e-43f5-9daa-4640053d5500', divine_agency_explanatory_necessity, deontological).
narrative_ontology:cs_axiom('5c368690-ab9e-43f5-9daa-4640053d5500', foundational, scriptural_authority_empirical_validity).
narrative_ontology:cs_axiom_status(scriptural_authority_empirical_validity, holdable).
narrative_ontology:cs_axiom_grounding('5c368690-ab9e-43f5-9daa-4640053d5500', scriptural_authority_empirical_validity, deontological).
narrative_ontology:cs_reference_frame('5c368690-ab9e-43f5-9daa-4640053d5500', scriptural_anthropogenic_authority).
narrative_ontology:cs_drift_state('5c368690-ab9e-43f5-9daa-4640053d5500', contemporary_scientific_consensus_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c368690-ab9e-43f5-9daa-4640053d5500', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_interpretation_tradition).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, secular_scientific_practitioners).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, public_school_biology_curricula).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, evolutionary_research_funding).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, faith_community_members).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, faith_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious institutions (churches, faith-based organizations) interpret the anthropological and paleontological record through scriptural frameworks and advance readings of human origins that center divine creation. They set the terms of interpretation within faith communities, adjudicate which empirical claims align with scripture, and establish institutional positions on evolutionary teaching. They benefit from maintaining the interpretive monopoly on origins questions within their constituencies.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_institutional_authority, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Theological scholars and exegetes who specialize in reconciling (or rejecting reconciliation with) evolutionary science maintain a coherent interpretive lineage grounded in scriptural authority. The tradition flourishes when scriptural testimony is treated as a valid epistemic source for origins claims. It is a doctrine system, not a single agent, but it collects legitimacy and institutional resources when the creationist reading is sustained.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_interpretation_tradition, beneficiary,
    organized, civilizational, identity_locked, national).

% Biologists, paleontologists, and anthropologists whose research and teaching assume evolutionary frameworks must constantly defend the legitimacy of that framework in public discourse and education policy. They bear the cost of disputed authority: their professional epistemic standing is contested in school board meetings, legislative debates, and public forums. They cannot simply pursue their research unmolested; they must continually re-establish the warrant for their methods.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, secular_scientific_practitioners, payer,
    institutional, biographical, constrained, national).

% Public education systems are caught between teaching the scientific consensus on evolution and accommodating parental and community objections rooted in creationist readings. Schools face pressure to include 'alternative explanations,' teach 'both sides,' or remove evolutionary content. The constraint imposes costs: curriculum design becomes contested, teacher autonomy is limited, and the teaching of science is subordinated to local interpretive disputes.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, public_school_biology_curricula, payer,
    moderate, biographical, constrained, national).

% Research funding for evolutionary biology, human paleoanthropology, and related fields is subject to legislative and public pressure to justify its legitimacy. Funding agencies and research institutions must defend evolutionary frameworks against organized skepticism. The constraint does not prevent funding but increases its friction: grants require stronger justification, legislative support is harder to maintain, and entire research programs can be defunded or de-prioritized based on creationist objections.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, evolutionary_research_funding, payer,
    powerful, biographical, constrained, national).

% Church members and believers are told they can read the anthropological record as confirming divine creation and need not accept the evolutionary timeline. They benefit from an interpretive framework that does not require them to choose between faith and understanding human origins. But they also pay by being excluded from scientific credentialing and professional pathways in fields that require acceptance of evolutionary theory, and by inheriting a cognitive burden: they cannot naively receive public science education without processing the counter-narrative first.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, faith_community_members, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, faith_community_members, payer).

% Universities, professional societies (e.g., National Academy of Sciences), and accreditation boards set the standards for what counts as valid natural-science reasoning. They enforce the requirement that credentialed scientific practice accept evolutionary frameworks. They benefit from the constraint insofar as it sustains the epistemic monopoly of materialist methods within the credentialed science domain, but they also pay enforcement costs: they must continually defend their epistemic standards against organized challenges and litigation.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, scientific_credentialing_bodies, agenda_setter,
    institutional, biographical, constrained, national).

% Indigenous knowledge systems that have their own accounts of human origins and continuous habitation are structurally excluded from the conversation. Neither the creationist reading nor the naturalist reading treats indigenous epistemologies as valid sources for understanding the anthropological record. They are the absent voice whose objections would reframe the entire question.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, excluded_indigenous_epistemologies, excluded,
    powerless, generational, trapped, national).

% Philosophers of science examine what counts as science, evidence, and legitimate explanation. They observe that the creationist reading and the naturalist reading operate under different epistemic frameworks and ask whether both can be valid within their respective domains or whether one framework must adjudicate all claims about origins.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, analytical_philosophy_of_science, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, religious_institutional_authority).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates believers into a coherent interpretive tradition where divine agency is recognized as a valid explanatory cause for the emergence of humans and complexity. Solves the theological problem of reconciling scriptural authority with the empirical record by authoritatively translating the record into terms compatible with faith. Provides religious communities with a unified framework for understanding human origins that does not require rejecting their foundational texts.
% TRANSFER_FUNCTION: Transfers epistemic authority from the materialist scientific methodology to scriptural interpretation within faith communities. In educational and public-policy contexts, it shifts resources (curricula time, legislative attention, litigation costs) from evolutionary-science advancement to defending the legitimacy of evolutionary claims. Within religious institutions, it consolidates power in interpretive authorities (theologians, institutional church leadership) who adjudicate what the record means.
% ABSENT_VOICES: Indigenous peoples whose own origin accounts and knowledge systems are systematically excluded from the contest between creationist and naturalist readings. They would argue that the anthropological record is their history, knowable through their own epistemologies and sustained oral transmission, and that neither Western religious nor Western secular frameworks should monopolize its interpretation. Scientific practitioners in non-Western contexts whose alternative epistemic traditions are also excluded.
% DISAPPEARANCE_RATIONALE: If the creationist reading as a sustained institutional constraint disappeared—if religious communities no longer advanced creationist interpretations of the record—public education would realign around evolutionary teaching without curricular dispute, research funding for evolutionary biology would face no organized epistemic challenge, and the cognitive burden on faith-community members of processing counter-narrative would ease (though their theological positions might shift). The social reorganization would be substantial.
% FOUNDING_PROBLEM: The apparent tension between scriptural accounts of creation and scientific accounts of human emergence. The founding problem is framed theologically: how can believers honor scriptural testimony while accounting for paleontological and anthropological evidence? The creationist reading answers by re-reading the evidence as compatible with scripture, or by rejecting the empirical claims as overreach of materialist method.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions and creationist scholars attest the problem is live and ongoing—that the tension between scripture and evolutionary claims is a persistent theological challenge. Secular scientists and philosophers of science attest the problem is misconceived—that there is no genuine tension if one recognizes the epistemic boundaries between theological and scientific claims. Legislative and school-board testimony shows the tension manifests as real institutional dispute, regardless of how philosophers adjudicate it.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval because the creationist reading's institutional enforcement machinery strengthens: school-board activism becomes more organized, legislative challenges to evolutionary teaching multiply, and litigation over curriculum increases. The constraint increasingly forces costs onto payers (scientists, educators, funders) without expanding the coordination function proportionally. Theater rises from 0.22 to 0.42 because much enforcement activity becomes performative: formal creationist arguments appear in policy documents and curricula, but their actual explanatory function degrades (the fossil record is not reinterpreted; alternative creationist mechanisms are not specified). Suppression rises from 0.58 to 0.71 because the institutional pressure to suppress materialist authority in educational and public contexts intensifies, even as the creationist positive case remains underdeveloped. Accessibility collapse is moderate (0.62) because believers retain the option of accepting evolutionary science within a compartmentalized faith framework, and scientists retain the option of working within credentialed institutions; neither seat is completely trapped. Resistance is substantial (0.58) because secular science resists the constraint actively through litigation, legislative testimony, and professional-standards enforcement. The measurements trace a pattern of extraction growth with increasing theatricality: the coordination function (reconciling faith and empirical record) is genuine but fixed, while enforcement intensity and performative activity expand.
 *
 * PERSPECTIVAL GAP:
 *   From the religious institutional seat, the constraint is experienced as successful coordination: believers are unified in an interpretive framework, theological authority is intact, and the reading has resisted materialist monopolization of origins discourse. The constraint appears to solve a real problem (reconciling faith and evidence) and to protect an essential tradition. From the scientific seat, the same constraint is experienced as suppression: their epistemic framework is subjected to organized delegitimation, their teaching is politically contested, and their research funding is precarious. They experience the constraint as extracting authority without justification. The engine computes these divergent types from the structural data: the religious seat's high-beneficiary position, low exit costs, and control over the constraint's definition produce a rope-neighborhood classification at that seat; the scientific seat's high-target position, constrained exits, and subjection to the constraint's enforcement produce a snare-neighborhood classification. The claimed type (tangled_rope) reflects the structure as viewed from a position that sees both seats: genuine coordination for the beneficiary, genuine extraction for the target, active enforcement to hold both in place.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutional authority is the agenda-setter (d near 0.0, full beneficiary): it defines the constraint, collects institutional legitimacy and consolidated power over origins interpretation, and has multiple exit options (it can moderate its claims, adopt accommodationist theologies, or retreat to purely theological claims). Creationist interpretation tradition benefits without operating the constraint (d near 0.2, net beneficiary). Faith community members sit near symmetric (d near 0.5): they benefit from the interpretive framework (no cognitive dissonance between faith and origins understanding) but pay through identity-lock and exclusion from certain credentialing pathways. Secular scientists are targets (d near 0.85, net target): they bear the cost of defending their epistemic legitimacy in public forums, face funding constraints, and cannot simply pursue their research without continual boundary-maintenance work. Educational systems and evolutionary-research funding are similarly near-target (d near 0.8). Scientific credentialing bodies are ambiguous: they are partly agenda-setters (they enforce the materialist standard) and partly payers (they incur enforcement costs and face legal challenges). The divergence in directionality between the religious agenda-setter (low d, low extraction experienced) and the scientific payer (high d, high extraction experienced) is the core seat divergence: the same constraint appears as legitimate coordination from the religious seat and as enforced suppression from the scientific seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The creationist reading's founding problem is the theological tension between scriptural authority and evolutionary science. At t0, the problem is genuinely live: faith communities experience real cognitive dissonance, and the creationist reading addresses it by authoritatively reconciling scripture and record. At t50, the founding problem has partially shifted. Within faith communities, many have adopted accommodationist or theistic-evolution frameworks that reduce the cognitive tension without requiring active creationist readings of the record. Among secular scientists, the tension is experienced as the problem of public science education and legislative legitimacy, not as a personal theological problem. The constraint persists (theater_ratio rises) even as the founding problem's salience declines for both its original constituencies. This is a mandatrophy candidate: the constraint began as coordination (solving the theological problem) and has evolved into extraction (defending religious authority against scientific credentialing). The coordination function is still present but diminished; the extraction function has grown. The six_questions verdict (disappearance_verdict: world_rearranges; founding_problem_status: contested) captures this: the founding problem is no longer universally conceded as live or solved, yet the constraint persists through institutional inertia and enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_boundary_negotiation,
    'Are the creationist and naturalist readings of the anthropological record genuinely addressing different domains (theology vs. science), or are they competing claims about the same empirical facts?',
    'Epistemological analysis of the structure of creationist claims: do they make falsifiable empirical predictions about the fossil record, genetic evidence, or archaeological data, or do they operate at the level of interpretation (reading the same evidence differently)? Do they propose novel mechanisms or reinterpret existing ones?',
    'If the readings address different domains (theology vs. science), the constraint is a coordination problem (how to honor both types of truth claims) and the type classification remains tangled_rope. If they compete over the same facts, the constraint is pure extraction of epistemic authority and the classification would shift toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_boundary_negotiation, conceptual, 'Whether creationist and naturalist readings occupy different epistemic domains or compete over identical empirical claims.').

omega_variable(
    faith_community_cognitive_burden,
    'Is the cognitive burden on faith-community members of processing the creationist reading a feature of the reading (necessary cost of honoring both faith and evidence) or a symptom of the constraint''s enforced suppression (cost of having to actively maintain a counter-narrative)?',
    'Post-constraint scenarios: in communities that have abandoned the creationist reading in favor of theistic evolution or compartmentalized faith, does the cognitive burden ease? Do cognitive-load studies show burden reduction when the counter-narrative is no longer actively enforced?',
    'If the burden eases without the constraint, the burden is symptom, not feature, and the extraction component is higher than the coordination function; this would support reclassification toward snare. If the burden persists (because the underlying theology requires reconciliation work), it is a true coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faith_community_cognitive_burden, empirical, 'Whether the creationist reading''s cognitive costs are inherent to genuine theological coordination or symptoms of enforced suppression.').

omega_variable(
    kernel_contest_framing,
    'Is the anthropological record a single kernel admitting multiple readings (three competing truth claims about the same facts), or are there really three separate kernels (three different question structures) being conflated under the label ''anthropological record''?',
    'Structural analysis of whether the three readings (creationist, naturalist, indigenous epistemology) are in genuine logical contention (would a decision in favor of one entail rejection of the others) or whether they operate under different epistemic and ontological frameworks such that they address different questions.',
    'If three separate kernels are present, this constraint story is incomplete: the constraint is one reading of one kernel (anthropological record as a question of scientific explanation), and the indigenous epistemology reading operates on a different kernel (anthropological record as a question of relational continuity knowable through oral tradition). The ε-invariance principle would require decomposing into separate stories. If one kernel genuinely admits three readings, the current framing is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether the anthropological record is one kernel with three readings or three separate kernels conflated under one label.').

omega_variable(
    institutional_identity_lock_mechanism,
    'What specific mechanisms bind religious institutional authority and creationist interpretation tradition to the creationist reading such that they cannot coherently shift to alternative readings (theistic evolution, evolutionary compatibility) without institutional dissolution?',
    'Historical and institutional analysis: which institutions have adopted alternative readings without collapse? What negotiations or identity work did they undertake? What constituencies did they lose, and what did they retain?',
    'If identity-lock is absolute, the constraint is nearly inescapable for religious institutions and the exit_options attribution (identity_locked) is correct. If some institutions have successfully navigated the shift, the identity-lock is partial and the exit options are less constrained than currently authored; this would modulate directionality and possibly reclassify the institutional-seat type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_mechanism, empirical, 'The strength and specificity of institutional identity-fusion to the creationist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__creationist_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(anth_tr_t10, observed).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__creationist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(anth_tr_t20, observed).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__creationist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(anth_tr_t30, observed).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__creationist_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(anth_tr_t40, observed).
narrative_ontology:measurement(anth_tr_t50, anthropological_record__creationist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(anth_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t10, anthropological_record__creationist_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(anth_be_t10, observed).
narrative_ontology:measurement(anth_be_t20, anthropological_record__creationist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(anth_be_t20, observed).
narrative_ontology:measurement(anth_be_t30, anthropological_record__creationist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(anth_be_t30, observed).
narrative_ontology:measurement(anth_be_t40, anthropological_record__creationist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(anth_be_t40, observed).
narrative_ontology:measurement(anth_be_t50, anthropological_record__creationist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(anth_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t10, anthropological_record__creationist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(anth_su_t10, observed).
narrative_ontology:measurement(anth_su_t20, anthropological_record__creationist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(anth_su_t20, observed).
narrative_ontology:measurement(anth_su_t30, anthropological_record__creationist_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(anth_su_t30, observed).
narrative_ontology:measurement(anth_su_t40, anthropological_record__creationist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(anth_su_t40, observed).
narrative_ontology:measurement(anth_su_t50, anthropological_record__creationist_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(anth_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The anthropological record is a contested kernel admitting multiple readings. The naturalist_reading and indigenous_epistemology_reading are sibling constraints addressing the same empirical domain through alternative epistemic frameworks. This creationist_reading constrains the institutional authority and public discourse around all three readings. The three stories are linked because shifts in one reading's institutional standing (e.g., increasing acceptance of evolutionary compatibility in religious communities) cascade to affect the others' social position and epistemic standing. Network edges run both upstream (this reading influences the credibility and institutional standing of the naturalist and indigenous readings) and horizontally (the sibling readings compete for legitimacy within the same discourse space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
