% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Westminster Parliamentary Supremacy (Crown Reading of Treaty of Waitangi)
 *   domain: constitutional/political/indigenous-rights
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is a contested constitutional kernel. The
 *   Crown Reading instantiated here interprets Article I English text ('Māori
 *   chiefs cede complete sovereignty to the Crown') as establishing
 *   Westminster parliamentary supremacy without requirement for Māori consent
 *   to Crown legislation or Māori co-determination over resources. This
 *   reading grounds Crown plenary authority to unilaterally allocate lands,
 *   resources, and governance over the past 184 years. It benefits the Crown
 *   executive and settler Parliament; it extracts from Māori iwi collectives
 *   who are subordinated to Crown legislative veto over all resource and
 *   governance claims. The reading coexists with two sibling readings
 *   (partnership reading treating the Treaty as establishing ongoing
 *   Crown-Māori partnership with consultation and protection requirements;
 *   rangatiratanga reading treating the Māori Article II as retaining full
 *   tino rangatiratanga—full authority—over Māori lands and resources). This
 *   story instantiates ONLY the Crown sovereignty reading, consistent with
 *   ε-invariance: a different reading would author a different constraint
 *   story with a different ε, different beneficiaries/victims, and different
 *   classification. The sibling readings are separate JSON files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Crown Executive: Holds unilateral power to set resource and governance policy under this reading; principal beneficiary and agenda-setter
 *   - Settler Parliament: Holds plenary legislative authority without Māori consent requirement; secondary beneficiary
 *   - Māori Iwi Collectives: Subject to unilateral resource allocation and legislative exclusion; identity-locked targets whose exit costs are totalized (abandonment of ancestral territory and identity framework)
 *   - English Legal Tradition (non-agent): Vindicated as the authoritative frame; the reading's operation validates Westminster doctrine of indivisible parliamentary sovereignty
 *   - Crown Courts: Interpreter of law; historically enforced the reading's straightforward sovereignty transfer framing, increasingly strained by partnership and rangatiratanga readings post-1975
 *   - Rangatiratanga and Partnership Reading Advocates: Excluded from co-authoring Crown constitutional doctrine until legislation post-1975 provides them limited purchase
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.82).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.71).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Westminster Parliamentary Supremacy (Crown Reading of Treaty of Waitangi)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/political/indigenous-rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '3fee2673-768a-483c-a3af-770748df0e0a').
narrative_ontology:cs_kernel_codification('3fee2673-768a-483c-a3af-770748df0e0a', fixed_text).
narrative_ontology:cs_authority_grounding('3fee2673-768a-483c-a3af-770748df0e0a', lineage).
narrative_ontology:cs_interpretation_layer_present('3fee2673-768a-483c-a3af-770748df0e0a').
narrative_ontology:cs_reading_relation('3fee2673-768a-483c-a3af-770748df0e0a', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fee2673-768a-483c-a3af-770748df0e0a', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('3fee2673-768a-483c-a3af-770748df0e0a', foundational, parliamentary_sovereignty_indivisible).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('3fee2673-768a-483c-a3af-770748df0e0a', parliamentary_sovereignty_indivisible, conventional).
narrative_ontology:cs_axiom('3fee2673-768a-483c-a3af-770748df0e0a', foundational, english_text_controls_interpretation).
narrative_ontology:cs_axiom_status(english_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('3fee2673-768a-483c-a3af-770748df0e0a', english_text_controls_interpretation, deontological).
narrative_ontology:cs_axiom('3fee2673-768a-483c-a3af-770748df0e0a', secondary, crown_unilateral_resource_allocation_authority).
narrative_ontology:cs_axiom_status(crown_unilateral_resource_allocation_authority, holdable).
narrative_ontology:cs_axiom_grounding('3fee2673-768a-483c-a3af-770748df0e0a', crown_unilateral_resource_allocation_authority, conventional).
narrative_ontology:cs_reference_frame('3fee2673-768a-483c-a3af-770748df0e0a', westminster_parliamentary_supremacy_1840).
narrative_ontology:cs_drift_state('3fee2673-768a-483c-a3af-770748df0e0a', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3fee2673-768a-483c-a3af-770748df0e0a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, english_legal_tradition).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_collectives).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_resource_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_crown_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises unilateral legislative and executive authority under this reading. Sets resource allocation policy, adjudicates land claims through Parliament, and implements policy without requirement for Māori consent or co-determination. Maintains this as the legitimate constitutional order grounded in Article I English text ceding complete sovereignty to the Crown.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Holds plenary legislative authority. Enacts laws affecting Māori resources, lands, and governance without Māori legislative veto or co-enactment requirement. Benefits from unilateral control over settlement policy, resource distribution, and constitutional amendment.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_parliament, beneficiary,
    institutional, generational, analytical, national).

% Subject to unilateral resource allocation, land confiscation, and legislative exclusion from co-determining policies that directly affect their iwi, lands (whenua), and cultural taonga. Exit from this arrangement means abandonment of ancestral territory, identity framework, and intergenerational claim structure — costs so totalized as to constitute non-exit. Suppression operates both through external legal barriers (parliamentary veto of all Māori-initiated legislation pre-1975; all-Pākehā parliament until 1868) and internalized through childhood education in English legal supremacy as 'natural' and 'inevitable.'
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_collectives, payer,
    powerless, generational, identity_locked, national).

% Seek redress for lands and resources allocated away under this reading. Their claims are heard by Crown-appointed bodies (Waitangi Tribunal pre-1975 was advisory-only, now fact-finding; Compensation Court operates under Crown statute) and remain subject to parliamentary override. They can litigate but all remedial authority is Crown-held.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_resource_claimants, payer,
    powerless, biographical, constrained, national).

% This reading vindicates the doctrine that sovereignty is indivisible and vested in Parliament; that treaties bind the Crown-in-Parliament but Parliament cannot bind itself; and that English common law is the default interpretive frame absent explicit supersession. The reading's operation validates this tradition as the legitimate authority ordering colonial governance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, english_legal_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(waitangi_sovereignty_allocation__crown_sovereignty_reading, english_legal_tradition).

% The institutional identity that signed Article I of the Treaty and claims it ceded complete sovereignty. Under this reading, the Crown holds the plenary authority Article I transferred, and that authority has descended to the Crown-in-Parliament (settler government). The Crown apparatus benefits from the sovereignty allocation and from the doctrine that treaties are executive instruments not legislative contracts.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_crown_apparatus, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_crown_apparatus, beneficiary).

% Māori scholars, jurists, and political movements that read the Treaty as establishing Crown-Māori partnership with ongoing consultation and protection requirements. They are excluded from co-authoring Crown policy and constitutional interpretation; their framing is subordinated in Crown courts and Parliament until legislation explicitly recognizes it (post-1975 Waitangi Tribunal; 1986 Rangatiratanga clause; 1990 Crown-Māori partnership framing in legislation — all represent partial breaks with the exclusion, not its end).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, rival_partnership_reading_advocates, excluded,
    moderate, generational, constrained, national).

% Interpret the Treaty within common law frameworks. Historically read Article I as straightforward sovereignty transfer; more recently (1980s onward) reframe around good faith and partnership principles, creating tension within the reading. They interpret law but do not author it; Parliament sets the ultimate frame.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legislative and executive authority (the Crown-in-Parliament) to coordinate colonial settlement, resource allocation, and law-making across a new polity. Solves the coordination problem of who decides binding policy in a society of settlers and indigenous inhabitants by vesting that authority entirely in the Crown.
% TRANSFER_FUNCTION: Moves resource allocation power, legislative authority, and constitutional veto from Māori iwi collectives (who held them pre-1840) to the Crown-in-Parliament. Concretely: moves ownership and control of land, minerals, fisheries, and governance authority from iwi to Crown, and vests the power to distribute or retain these resources in the settler Parliament.
% ABSENT_VOICES: Rangatiratanga reading advocates (who interpret Article II's Māori text as retaining full authority over lands and taonga for Māori, with Crown gaining only executive authority over settlers) are structurally excluded from co-authoring Crown constitutional doctrine. Partnership reading advocates are partially excluded until legislation post-1975 provides them limited purchase. The reading itself forecloses their framings from the authoritative constitutional space until Parliament or courts unilaterally recognize them.
% DISAPPEARANCE_RATIONALE: Crown reading adherents attest that if this sovereignty allocation disappeared, Crown authority would collapse and the polity would fragment (settler government could not function without plenary legislative power). Partnership and rangatiratanga reading advocates attest that if this reading disappeared and were replaced by their framings, Crown authority would transform (not collapse) — Crown would govern settlers through parliamentary law but would require Māori co-determination over Māori resources, taonga, and iwi governance. The disappearance verdict hinges on whether one accepts the Crown reading's premise that sovereignty must be plenary and indivisible.
% FOUNDING_PROBLEM: The founding problem this reading was built to solve: establishing a unified sovereign authority capable of governing a settler colony in a territory with an existing indigenous polity. The Crown reading solves it by vesting all sovereignty in the Crown, which delegates to the settler Parliament.
% FOUNDING_PROBLEM_CORROBORATION: The Crown (NZ government) and English legal historians affirm the founding problem was real: pre-1840, British settlers had no unified law-making authority and Māori iwi held independent governance and resource authority; the Treaty established a single sovereign (the Crown) to order colonial governance. Partnership and rangatiratanga reading advocates contest this: they affirm the coordination problem existed but deny that plenary Crown sovereignty was the only solution — they attest that shared sovereignty or Māori retention of resource authority could have solved the same coordination problem without extraction. Independent historical analysis (e.g., Annie Coombes on Treaty interpretation, Paul McHugh on indigenous rights doctrine) confirms the coordination problem existed but documents the readings as genuinely contested, not convergent.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the Crown reading vests unilateral resource allocation and legislative authority in Crown hands without Māori consent mechanism or co-determination requirement. The beneficiaries (Crown executive, settler Parliament) collect rents from this allocation—specifically, the capacity to govern Māori resources unilaterally. The victims (Māori iwi, resource claimants) bear the cost: their historical governance and resource authority was transferred to Crown control, and all subsequent claims for redress are heard by Crown-appointed bodies and remain subject to parliamentary override. Suppression is high (0.71) because the Crown reading's persistence depends on active exclusion of rival readings from authoritative constitutional space—court interpretation was historically locked to the straightforward sovereignty transfer framing, Māori political voice was excluded from Parliament until 1868 (and remained proportionally tiny until post-1975 reforms), and all indigenous-led alternatives to Crown plenary authority were treated as legal nullities. Theater ratio is moderate (0.44) and has risen significantly over time: the coordination function (establishing unified sovereign authority) was genuine in 1840, but over the interval the Crown's enforcement activity has shifted toward defending resource monopoly rather than coordinating governance—by 2024 the enforcement is increasingly theatrical (Waitangi Tribunal findings are non-binding, Crown consultation is advisory, the core doctrine of parliamentary supremacy persists unchanged despite acknowledged harm). The measurement series show extractiveness stable (~0.82-0.88 across 184 years) with slight decline post-1970s when partnership framing entered legislation (but the core extraction—unilateral Crown resource allocation—persists). Theater ratio rises from 0.15 to 0.44 (rising performative activity: Commission of Inquiry, Waitangi Tribunal, treaty settlement framework) suggesting Goodhart drift (Crown governance theater substituting for actual power-sharing). Suppression requirement declined from 0.84 to 0.71 (enforcement hardened in mid-20th century, eased after 1975 reforms) suggesting that as the reading's legitimacy was openly contested, active suppression became necessary to maintain it. The single shared time grid (23-year intervals across 184 years of Treaty history) aligns all metrics.
 *
 * PERSPECTIVAL GAP:
 *   The Crown executive/settler Parliament seat computes the constraint as rope (genuine coordination function: establishing unified sovereign authority; participants are net beneficiaries; alternatives are not suppressed—from their perspective, the alternative was chaos and overlapping governance). The Māori iwi seat computes the constraint as snare (coordination story as cover for resource extraction; the Crown's unified authority is exercised to subordinate Māori resource claims; alternatives are actively suppressed—Māori autonomous governance was destroyed; participation in Crown legislative process was denied pre-1868 and remains minoritized). This divergence is structural and reflects genuine incompatibility: the Crown reading cannot acknowledge the asymmetry without ceasing to justify parliamentary supremacy as natural coordination. The engine computes per-seat classification from the authored structural data (beneficiary/victim + exit + power + time_horizon + spatial_scope); the divergence emerges from the structure, not from subjective disagreement about identical facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown executive and settler Parliament: beneficiaries (collect resource allocation authority and legislative veto power; d near 0.0, full beneficiary). Māori iwi collectives: targets (lose historical governance and resource authority; constrained by identity-locked exit—abandonment of iwi identity, ancestral territory, and intergenerational claim structure carries costs so totalized as to constitute non-exit; d near 1.0, full target). Māori resource claimants: targets (appeal to Crown-appointed bodies for redress; all remedial authority is Crown-held; constrained exit—they can abandon claims but that means abandonment of resource interests; d high, near 0.8). English legal tradition: non-agent (vindicated, not beneficiary—it collects no rents; included in vindicated_propositions, not beneficiaries). The derivation chain follows from the structural data: beneficiaries hold institutional power, arbitrage-grade exit, and reap the constraint's gains; targets hold powerless/moderate power, identity-locked or constrained exit, and bear the constraint's costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The Crown reading instantiates a Tangled Rope classification: (1) genuine coordination function—the founding problem was real (pre-1840 British settlers had no unified law-making authority; Māori iwi held independent governance)—and the Crown reading solved it by vesting unified sovereignty in the Crown; (2) asymmetric extraction—the same unified authority was used to unilaterally allocate resources away from Māori iwi toward Crown and settler use, subordinating Māori interests to parliamentary will; (3) active enforcement—the reading's persistence depends on Crown courts interpreting Article I straightforwardly (against the Māori text, which retained tino rangatiratanga), on Parliament excluding rival readings from legislative consideration, and on suppressing alternatives (indigenous governance authorities destroyed; Māori political exclusion maintained until 1868 and minoritized thereafter). The classification prevents mislabeling pure extraction as coordination: the reading is NOT pure snare because the coordination function is genuine and was needed; it is NOT pure rope because the extraction is asymmetric and active suppression is required to prevent Māori reclamation of resource authority. The mandate (Article I ceded complete sovereignty) is live and contested (post-1975 partnership framing and rangatiratanga reading advocates deny the mandate's legitimacy), but the classification does not resolve the contest—it measures how the Crown reading operationalizes the mandate. The theater ratio rising over time (from 0.15 to 0.44) suggests potential mandatrophy: the founding coordination problem is substantially solved (government is unified and stable), yet enforcement activity has shifted toward defending the reading's legitimacy rather than performing its coordination function. The Waitangi Tribunal and treaty settlement framework are formally non-binding and advisory—they perform consultation without transferring power—suggesting that as the reading's mandated function atrophied (unified government is established; the only remaining activity is resource retention), enforcement shifted to maintaining the reading's institutional theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_i_cession_ambiguity,
    'Does Article I English text (''Māori chiefs cede complete sovereignty'') unambiguously establish plenary Crown parliamentary supremacy, or is the text itself contested under principles of treaty interpretation (ambiguity resolved against the drafter, good faith interpretation)?',
    'Jurisprudential analysis of treaty interpretation doctrine (Vienna Convention on the Law of Treaties principles, New Zealand courts'' evolution post-1975). The Crown reading assumes straightforward English text dominates; alternative readings assert that good faith interpretation requires the Māori text (Article II tino rangatiratanga) to carry equal weight or that ambiguity must be resolved against the Crown as drafter.',
    'If Article I is deemed unambiguous by straightforward English interpretation, the Crown reading holds and Māori authority is subordinate. If the text is deemed ambiguous under good faith interpretation, or if the Māori text carries equal authority, the entire classification could shift from Tangled Rope toward partnership or rangatiratanga reading—the ε referent itself (the standing arrangement under contest) would change to center on an alternative arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_i_cession_ambiguity, conceptual, 'Whether the Crown reading''s textual anchor (Article I English sovereignty cession) is genuinely unambiguous or contestable under treaty interpretation doctrine.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (legal barriers, political exclusion, institutional veto) or internalized (Māori agents accepting the reading''s legitimacy as natural law—''this is just how government works'')?',
    'Post-legal-change test: If Māori political representation and court recognition of partnership/rangatiratanga readings increase (as post-1975), does measured suppression decline? If suppression persists despite legal changes, the mechanism is significantly internalized (childhood education in Westminster supremacy, career incentives for assimilation, professional identity fusion with English legal tradition). If suppression declines rapidly with legal change, the mechanism was primarily structural.',
    'If suppression is primarily structural, removing legal barriers (Māori veto power, binding treaty settlement, constitutional co-determination) would reduce the reading''s extractiveness significantly. If suppression is internalized, the reading could persist in attenuated form even after legal equality—the target agents would carry the suppression with them. The measurement series show suppression declining from 0.84 to 0.71 (post-1975 reforms), suggesting partial structural mechanism; however, the reading persists unchanged in constitutional authority despite legal changes, suggesting significant internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the constraint''s suppression operates through external barriers (removable by law change) or internalized frames (persisting after barrier removal).').

omega_variable(
    coordination_vs_extraction_separability,
    'Is unified sovereign authority (the coordination function) inseparable from Westminster parliamentary supremacy without Māori consent (the extraction mechanism)? Could the founding coordination problem have been solved by shared sovereignty or Māori retention of resource authority without sacrificing unified government?',
    'Historical counterfactual analysis and comparative constitutional models: Would a co-determination arrangement (shared legislative veto, Māori resource control with Crown executive authority over settlers, bicameral structure with Māori chamber) have achieved stable unified governance? Contemporary evidence from other post-colonial jurisdictions (Canada, Australia post-Mabo) suggests shared sovereignty can provide both coordination and constraint on extraction, implying the functions are separable.',
    'If the functions are separable, the extraction is pure rent-seeking riding on a legitimate coordination function—the constraint should be reclassified or the ε should be adjusted upward to reflect that only a portion is necessary coordination cost. If the functions are inseparable (unified government requires unilateral Crown authority), the extraction is the necessary price of coordination—the constraint remains Tangled Rope with high but justifiable extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_separability, conceptual, 'Whether unified government required unilateral Crown authority or whether shared sovereignty could have provided both coordination and Māori resource protection.').

omega_variable(
    kernel_reading_legitimacy_contest,
    'Which reading of the Treaty kernel is normatively legitimate: Crown reading (complete sovereignty), partnership reading (ongoing consultation), or rangatiratanga reading (Māori authority retained)? This question is explicitly NOT resolvable by the constraint classification engine—it is a normative commitment question that different political and legal traditions answer differently.',
    'Not resolvable by structural analysis. Different legal traditions (Westminster supremacy, indigenous rights doctrine, constitutional democracy principles) offer incompatible answers. The constraint classification engine measures the Crown reading''s structure as Tangled Rope; it does not adjudicate whether Tangled Rope is normatively acceptable, required, or unjust. Political and legal change (legislation, court doctrine shifts, constitutional amendment) are the decision mechanisms.',
    'This omega documents that the constraint story itself carries no normative verdict on the reading''s legitimacy. Authors and consumers of this constraint story should recognize that choosing the Crown reading as ''the constraint'' is a committer choice (one reading of the kernel); it is not a discovery of natural fact. The engine computes per-seat classification; it does not resolve the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy_contest, preference, 'Recognizing that the choice of reading (Crown sovereignty, partnership, or rangatiratanga) reflects committer values, not structural discovery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wait_tr_t23, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 23, 0.18).
narrative_ontology:measurement(wait_tr_t46, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 46, 0.28).
narrative_ontology:measurement(wait_tr_t69, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 69, 0.38).
narrative_ontology:measurement(wait_tr_t92, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 92, 0.42).
narrative_ontology:measurement(wait_tr_t115, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 115, 0.46).
narrative_ontology:measurement(wait_tr_t138, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 138, 0.44).
narrative_ontology:measurement(wait_tr_t161, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 161, 0.45).
narrative_ontology:measurement(wait_tr_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 184, 0.44).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(wait_be_t23, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 23, 0.89).
narrative_ontology:measurement(wait_be_t46, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 46, 0.84).
narrative_ontology:measurement(wait_be_t69, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 69, 0.82).
narrative_ontology:measurement(wait_be_t92, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 92, 0.79).
narrative_ontology:measurement(wait_be_t115, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 115, 0.81).
narrative_ontology:measurement(wait_be_t138, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 138, 0.8).
narrative_ontology:measurement(wait_be_t161, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 161, 0.82).
narrative_ontology:measurement(wait_be_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 184, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(wait_su_t23, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 23, 0.84).
narrative_ontology:measurement(wait_su_t46, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 46, 0.79).
narrative_ontology:measurement(wait_su_t69, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 69, 0.74).
narrative_ontology:measurement(wait_su_t92, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 92, 0.68).
narrative_ontology:measurement(wait_su_t115, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 115, 0.72).
narrative_ontology:measurement(wait_su_t138, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 138, 0.71).
narrative_ontology:measurement(wait_su_t161, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 161, 0.71).
narrative_ontology:measurement(wait_su_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 184, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=184
narrative_ontology:measurement(wait_grid_01, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(class), 0, 0.85).
narrative_ontology:measurement(wait_grid_02, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(class), 184, 0.68).
narrative_ontology:measurement(wait_grid_03, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(individual), 0, 0.82).
narrative_ontology:measurement(wait_grid_04, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(individual), 184, 0.62).
narrative_ontology:measurement(wait_grid_05, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(organizational), 0, 0.88).
narrative_ontology:measurement(wait_grid_06, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(organizational), 184, 0.71).
narrative_ontology:measurement(wait_grid_07, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(structural), 0, 0.92).
narrative_ontology:measurement(wait_grid_08, waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse(structural), 184, 0.78).
narrative_ontology:measurement(wait_grid_09, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(class), 0, 0.28).
narrative_ontology:measurement(wait_grid_10, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(class), 184, 0.71).
narrative_ontology:measurement(wait_grid_11, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(individual), 0, 0.18).
narrative_ontology:measurement(wait_grid_12, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(individual), 184, 0.62).
narrative_ontology:measurement(wait_grid_13, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(organizational), 0, 0.31).
narrative_ontology:measurement(wait_grid_14, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(organizational), 184, 0.68).
narrative_ontology:measurement(wait_grid_15, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(structural), 0, 0.22).
narrative_ontology:measurement(wait_grid_16, waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance(structural), 184, 0.72).
narrative_ontology:measurement(wait_grid_17, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(class), 0, 0.86).
narrative_ontology:measurement(wait_grid_18, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(class), 184, 0.69).
narrative_ontology:measurement(wait_grid_19, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(individual), 0, 0.83).
narrative_ontology:measurement(wait_grid_20, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(individual), 184, 0.63).
narrative_ontology:measurement(wait_grid_21, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(organizational), 0, 0.87).
narrative_ontology:measurement(wait_grid_22, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(organizational), 184, 0.72).
narrative_ontology:measurement(wait_grid_23, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(structural), 0, 0.91).
narrative_ontology:measurement(wait_grid_24, waitangi_sovereignty_allocation__crown_sovereignty_reading, stakes_inflation(structural), 184, 0.79).
narrative_ontology:measurement(wait_grid_25, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(class), 0, 0.77).
narrative_ontology:measurement(wait_grid_26, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(class), 184, 0.71).
narrative_ontology:measurement(wait_grid_27, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(individual), 0, 0.71).
narrative_ontology:measurement(wait_grid_28, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(individual), 184, 0.69).
narrative_ontology:measurement(wait_grid_29, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(organizational), 0, 0.81).
narrative_ontology:measurement(wait_grid_30, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(organizational), 184, 0.68).
narrative_ontology:measurement(wait_grid_31, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(structural), 0, 0.89).
narrative_ontology:measurement(wait_grid_32, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression(structural), 184, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.2).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_land_confiscation__crown_authority).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, new_zealand_parliamentary_supremacy).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel decomposes into three structurally distinct constraints, each with its own ε, beneficiary/victim structure, and classification. The Crown sovereignty reading (this story) establishes Westminster parliamentary supremacy and unilateral resource allocation. The partnership reading establishes consultation and protection obligations. The rangatiratanga reading establishes Māori baseline authority over taonga and lands with Crown as executive agent. These are not three interpretations of one constraint—they are three different constraints grounded in different framings of the same contested kernel. They are linked via network.affects_constraints to document the family relationship. Each story's ε referent is the standing arrangement under contest AS THAT READING SEES IT: the Crown reading's ε measures extraction from Crown supremacy; the partnership reading's ε would measure extraction from partnership breach; the rangatiratanga reading's ε would measure extraction from Māori authority violation. Reading-indexed values over a fixed referent (OQ-26, OQ-258).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
