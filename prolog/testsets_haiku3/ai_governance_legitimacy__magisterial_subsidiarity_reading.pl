% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: AI Governance Legitimacy: Magisterial Subsidiarity Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint represents ONE READING of a contested kernel around AI
 *   governance legitimacy. The magisterial-subsidiarity reading asserts that
 *   legitimate AI governance must conform to Catholic Social Doctrine
 *   principles: common good, subsidiarity (decisions at the most local
 *   competent level), solidarity (mutual interdependence and support), and
 *   universal destination of goods (AI-generated value serves all, not
 *   monopolistic extraction). This reading grounds legitimacy in
 *   authoritative Magisterium interpretation of these principles as applied
 *   to technology. The constraint entangles genuine coordination
 *   (accountability mechanisms that prevent technological power from
 *   consolidating) with asymmetric extraction (subordinating tech monopolies
 *   and military institutions to dignity-based constraints they did not
 *   author). Beneficiaries are workers, Global South populations, families,
 *   and vulnerable groups who gain governance authority and protection
 *   against algorithmic harms. Payers are unaccountable tech monopolies,
 *   military-industrial structures, and extractive finance systems that lose
 *   unilateral control over deployment and profit models. The claim/metric
 *   divergence is deliberate: the constraint is CLAIMED as tangled_rope
 *   (coordination + enforcement) while extractiveness is moderate (not a pure
 *   snare) because the coordination function—subordinating technology to
 *   dignity—is genuine, even as the constraint imposes real costs on
 *   institutional payers who resist it.
 *
 * KEY AGENTS:
 *   - magisterium_catholic_teaching_office: Sets legitimacy criteria grounded in Catholic Social Doctrine; authoritatively interprets subsidiarity, solidarity, common good, universal destination of goods as applied to AI governance.
 *   - workers_dignity_centered: Organized beneficiary with constrained exit; benefit from governance protecting meaningful work and wage preservation; bear costs of slower innovation deployment.
 *   - global_south_populations: Powerless but structurally significant beneficiary; trapped in global AI economy; benefit from resistance to algorithmic colonialism and extraction; bear cost of constrained technological access.
 *   - marginalized_vulnerable: Identity-locked beneficiary; structurally dependent on AI systems for basic access; benefit from algorithmic accountability and bias auditing; cannot exit the systems that govern their lives.
 *   - unaccountable_tech_monopolies: Institutional payer; direct target of extraction (loss of unilateral governance authority); cannot exit without abandoning profitable markets.
 *   - military_industrial_complex: Institutional payer; restricted in autonomous-weapons and surveillance-technology development; subordinated to civilian oversight grounded in dignity ethics.
 *   - extractive_finance_systems: Institutional payer; restricted in algorithmic predation and high-frequency trading; constrained by universal-destination-of-goods principle.
 *   - secular_liberal_governance_structures: Excluded from interpretive authority; would argue for democratic pluralism and secular sources of legitimacy; remain powerful but formally subordinated in this reading's framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.48).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.42).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy: Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'a0702599-b608-4d42-8dbf-2d5f6b7f5ea5').
narrative_ontology:cs_kernel_codification('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', formalized).
narrative_ontology:cs_authority_grounding('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', lineage).
narrative_ontology:cs_interpretation_layer_present('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5').
narrative_ontology:cs_reading_relation('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', foundational, human_dignity_constrains_efficiency).
narrative_ontology:cs_axiom_status(human_dignity_constrains_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', human_dignity_constrains_efficiency, deontological).
narrative_ontology:cs_axiom('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', foundational, subsidiarity_governance_distribution_required).
narrative_ontology:cs_axiom_status(subsidiarity_governance_distribution_required, holdable).
narrative_ontology:cs_axiom_grounding('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', subsidiarity_governance_distribution_required, deontological).
narrative_ontology:cs_axiom('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', secondary, universal_destination_of_goods_principle).
narrative_ontology:cs_axiom_status(universal_destination_of_goods_principle, holdable).
narrative_ontology:cs_axiom_grounding('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', universal_destination_of_goods_principle, deontological).
narrative_ontology:cs_reference_frame('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', dignity_centered_technology_governance).
narrative_ontology:cs_drift_state('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', contemporary_corporate_ai_deployment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a0702599-b608-4d42-8dbf-2d5f6b7f5ea5', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_dignity_centered).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, family_structures).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_vulnerable).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, unaccountable_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocacy_networks).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_dignity_centered).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Catholic Social Doctrine and issues encyclicals, apostolic exhortations, and statements on technology. Claims interpretive authority grounded in the living tradition of Catholic teaching on human dignity and the common good. Sets the substantive legitimacy criteria: technology must be subordinated to dignity, subsidiarity must govern authority distribution, solidarity must shape solidarity-based resource access, and the universal destination of goods must protect against monopolistic capture.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium_catholic_teaching_office, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from governance frameworks that subordinate AI automation to human dignity, that require technology adoption to preserve meaningful work, that protect wages and working conditions from algorithmic degradation, and that center worker participation in technological decisions affecting their livelihoods. They also bear a diffuse cost: slower innovation deployment, higher production costs passed through, and the organizational overhead of participatory accountability structures. Their exit option is severely constrained by global labor market integration.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_dignity_centered, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_dignity_centered, payer).

% Benefit from governance that resists algorithmic colonialism, that ensures AI-driven resource extraction serves the universal destination of goods rather than concentrating wealth in Northern tech centers, that protects against surveillance and behavioral manipulation, and that requires subsidiarity-respecting technology transfer that strengthens local governance capacity rather than imposing Northern technical solutions. They bear the cost of having governance authority over their own technological futures constrained by whatever framework emerges globally. Their exit is structurally trapped: they cannot opt out of the global AI economy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, payer).

% Benefit from governance that protects family autonomy and privacy from algorithmic surveillance, that restricts AI-driven attention capture and manipulative design targeting children, that subordinates efficiency metrics to family flourishing, and that enforces accountability when algorithms degrade family relationships or undermine parental authority. They carry a cost in restricted data utility for family-oriented services and slower development of family-helping technologies that could optimize convenience. Exit is constrained by embedding of digital technologies in basic infrastructure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, family_structures, beneficiary,
    moderate, generational, constrained, global).

% Benefit from governance frameworks that require algorithmic systems to be audited for bias, that protect against discriminatory pricing and service denial, that mandate transparency when algorithms affect access to housing, credit, healthcare, or justice, and that establish remedies when systems cause dignitary harm. Exit is identity-locked: they cannot step outside algorithmic systems that govern their access to basic services; non-compliance means structural exclusion from essential infrastructure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_vulnerable, beneficiary,
    powerless, biographical, identity_locked, global).

% Bear the constraint as a direct cost: required accountability mechanisms, participation mandates, subsidiary governance distribution, and prohibition of extractive data collection reduce unilateral control over technological deployment. They cannot exit without abandoning profitable markets. The constraint directly threatens their business model of innovation-without-accountability. They are not victims in the sense of bearing diffuse costs; they are targets of intentional extraction of governance authority and data-collection rights.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, unaccountable_tech_monopolies, payer,
    powerful, biographical, constrained, global).

% Bears the constraint through restrictions on autonomous weapons development, surveillance technology, and algorithmic targeting systems grounded in human-dignity ethics. The subsidiarity and common-good principles directly challenge the legitimacy of deploying AI without civilian oversight. Cannot exit without forgoing technological advantage or facing ecclesial and civil-society opposition. The constraint is not a marginal efficiency cost but a fundamental challenge to the authority structure of military technology governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Bear the constraint through restrictions on algorithmic predation, discriminatory pricing, and high-frequency trading systems that extract value without serving productive ends. The universal-destination-of-goods principle directly contradicts finance-driven extractive AI use cases. Cannot exit without abandoning entire business models. The constraint challenges the authority of financial institutions to set AI governance unilaterally.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_systems, payer,
    institutional, biographical, constrained, global).

% Benefit from governance frameworks grounded in human dignity and common-good principles, which amplify their advocacy voice and provide moral warrant for demanding accountability from tech monopolies. They can mobilize the constraint's legitimacy language to pressure governments and companies. Their exit option is mobile: they can advocate alternative governance framings or reduce engagement with the magisterial narrative.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Are formally excluded from the interpretive authority granted to the Magisterium in this reading's legitimacy framework. They would argue for democratic deliberation, pluralist value-balancing, and secular sources of legitimacy for tech governance. Their exclusion from the narrative does not remove them from power—governments remain the primary enforcement agents for tech policy—but it does mean their authority to set governance criteria is explicitly subordinated to Catholic Social Doctrine in this reading's framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_liberal_governance_structures, excluded,
    institutional, generational, constrained, global).

% Are excluded from the claim that Catholic Social Doctrine holds unique or primary interpretive authority over AI governance legitimacy. They would offer competing frameworks (Islamic ethics, Buddhist philosophy, secular humanism, Confucian thought) for thinking about technology and human flourishing. They remain present in global governance conversations but are backgrounded in this reading's authority structure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, competing_religious_and_philosophical_traditions, excluded,
    moderate, generational, mobile, global).

% Take an analytical position relative to the constraint: they can implement the accountability, transparency, and participatory governance mechanisms the framework demands, but they do not set the legitimacy criteria. Their role is to translate ethical mandates into technical architecture. They can resist implementation or advocate for alternative framings, but they operate within governance boundaries set elsewhere.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, technical_expertise_communities, observer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global accountability mechanism for AI development and deployment grounded in substantive human-dignity principles rather than mere efficiency metrics. Solves the coordination problem of preventing technological power from consolidating into unaccountable monopolies and military-industrial structures. Establishes a moral-authority framework that compels transparency about how algorithms affect workers, families, marginalized populations, and the Global South.
% TRANSFER_FUNCTION: Transfers governance authority from unilateral tech-monopoly and military-industrial decision-making to distributed, participatory structures that include worker representation, civil society oversight, and Magisterium-grounded ethical review. Moves algorithmic resource extraction from private monopolies to frameworks that treat AI-generated value as subject to the universal destination of goods. Extracts from tech monopolies and military institutions the authority to deploy AI without accountability; transfers that authority to structures that subordinate technology to human dignity.
% ABSENT_VOICES: Secular liberals, democratic pluralists, market libertarians, and competing religious traditions are structurally excluded from interpretive authority in this reading. They would argue that grounding AI governance legitimacy in a single religious tradition—even one with sophisticated social teaching—violates liberal principles of public reason, democratic self-determination, and pluralist value neutrality. They would also be in the governance conversation but formally subordinated rather than equal participants.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, tech monopolies would consolidate algorithmic power without accountability to worker dignity, Global South autonomy, family privacy, or vulnerable-population protection. Military AI would deploy without civilian oversight grounded in common-good principles. Finance would extract through algorithmic means without resistance grounded in solidarity ethics. Governments would lose a major moral-authority narrative for regulating tech platforms. Civil society would lose warrant for demanding accountability rooted in something deeper than consumer preference. The world does not rearrange overnight, but governance authority recenters on efficiency and market logic rather than dignity-centered subsidiarity.
% FOUNDING_PROBLEM: Artificial intelligence is being developed and deployed by private monopolies and military institutions without governance structures that subordinate technology to human dignity, subsidiarity, solidarity, and the common good. Algorithms encode values (efficiency, profit, control) that contradict Catholic Social Doctrine's central commitments. Workers are displaced without support; the Global South is subject to algorithmic colonialism; families are surveilled; vulnerable populations are discriminated against. The founding problem is the uncontrolled technological power of institutions that have no accountability to the dignity-based values Catholic teaching requires.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium itself attests the founding problem (multiple papal statements, diocesan letters, Vatican technology offices). Workers' rights organizations, Global South advocacy networks, family-advocacy groups, and marginalized-communities organizations independently attest algorithmic harms described as violating dignity and subsidiarity principles. Technologists and independent researchers confirm that AI systems deployed by monopolies lack accountability mechanisms for bias, discrimination, and surveillance. The problem is NOT corroborated only by the Magisterium; it is corroborated by independent witnesses outside the Catholic tradition—though the Magisterium's framing in dignity-and-subsidiarity language is its particular contribution.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate-high because the constraint directly subordinates powerful institutional actors (tech monopolies, military, finance) to governance frameworks they did not author and do not control. They experience real cost: restrictions on deployment, requirement for accountability, subordination of efficiency to dignity, redistribution of governance authority. But extractiveness is not extreme (not 0.75+) because the constraint does not involve pure coercion or complete suppression of alternatives—it operates through moral authority, civil-society mobilization, and governance leverage rather than through state force or property-law enforcement. Suppression (0.42) is moderate because the constraint requires active enforcement (disruption of tech monopoly unilateralism, military oversight, finance-sector restrictions) but that enforcement is rooted in recognized moral authority rather than raw coercion. Parties can resist and do; civil society must mobilize continuously; the constraint persists because it is grounded in principles that command allegiance. Theater_ratio (0.28) is low-moderate: the accountability and participatory-governance functions are real, but some enforcement energy goes into maintaining the dignity-centered narrative against technocratic counter-narratives, and some energy is performative (companies adopting 'ethics' language while preserving extractive practices). Accessibility_collapse (0.65) is moderate-high: tech monopolies have constrained alternatives if they want to operate in jurisdictions where this reading gains institutional backing, but they can migrate to other jurisdictions or invest in defeating the narrative; Global South populations have very restricted alternatives (trapped); workers have constrained alternatives but can organize collective resistance. Resistance (0.58) is moderate-high: tech monopolies actively resist through lobbying, counter-narrative, regulatory arbitrage; military institutions resist through security arguments; civil society pushes back by demanding actual enforcement; workers mobilize around participation requirements. The time-series measurements (all on one shared grid at t=0,5,10,15,20,25) show extractiveness rising from 0.38 to 0.48 then plateauing—the constraint strengthens as institutional backing accumulates but then stabilizes as tech actors adapt and counter-mobilize. Suppression_requirement stays stable at mid-level (the moral-authority enforcement mode does not require escalating coercion to maintain). Theater_ratio rises and then stabilizes, suggesting that initial performative adoption by tech companies gives way to either genuine compliance or rhetorical attrition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (tech monopolies, military, finance) and the agenda-setter seat (Magisterium) experience this constraint radically differently. From the Magisterium's position, the constraint is establishing legitimate governance grounded in enduring principles of human dignity and common good—it is the correction of a wrong authority structure that has allowed technology to escape subordination to human values. From the tech-monopoly seat, the constraint is an illegitimate imposition of a single tradition's values onto governance structures that should be neutral between competing ethical frameworks. From the worker seat, the constraint is both real protection (they have governance voice they lacked before) and limited (it solves the participation problem but does not solve unemployment or wage stagnation). From the Global South seat, the constraint is potential liberation (subsidiarity could enable local governance autonomy) but also risk (magisterial authority could be another form of external imposition). The engine computes these divergences from structural data: the Magisterium's high power + analytical exit + global scope point toward low directionality (beneficiary end); tech monopolies' high power + constrained exit + global scope but now-constrained authority point toward high directionality (target end); workers' organized power + constrained exit + global scope point toward moderate-high directionality (moderate target). These are not differences of opinion but structural differences in what the constraint gives and takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from beneficiary/victim declarations and exit options. Magisterium: role=agenda_setter, power=institutional, exit_options=analytical → low directionality (d ≈ 0.15–0.25), benefits from being the recognized authority. Tech monopolies: role=payer, power=powerful, exit_options=constrained (cannot leave profitable markets globally even under constraint) → high directionality (d ≈ 0.75–0.85), direct targets of governance extraction. Workers: role=beneficiary (governance voice) + secondary role=payer (slower innovation), power=organized, exit_options=constrained → moderate directionality (d ≈ 0.45–0.55), mixed position. Global South: role=beneficiary (subsidiarity autonomy), power=powerless, exit_options=trapped → moderate directionality (d ≈ 0.50–0.65), beneficiary role offset by trapped exit. Military: role=payer, power=institutional, exit_options=constrained → high directionality (d ≈ 0.75–0.85). Finance: role=payer, power=institutional, exit_options=constrained → high directionality (d ≈ 0.75–0.85). No directionality overrides needed; the derivation captures the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mission obsolescence) risk exists here: if the founding problem (uncontrolled technology power violating dignity) is solved, does the constraint persist as theater? The founding_problem_status is LIVE: algorithmic harms, monopolistic control, and lack of worker/Global South governance voice continue. But the founding_problem_corroboration is externally grounded—not just Magisterium assertion but independent attestation from tech workers, Global South advocacy, civil society, and technologists. The constraint is not yet mandatrophic. However, the risk scenario exists: if tech monopolies can create the appearance of accountability (ethics boards that have no power, participatory theater, public commitments to dignity unaccompanied by structural change), and if governments adopt dignity-language without structural enforcement, the constraint could become piton-like (performative maintenance, low real function). The theater_ratio trajectory (rising then plateauing at 0.28) hints at this risk: early period shows enthusiasm and apparent implementation; later period shows stabilization at a level where theater and function coexist. Mandatrophy declaration is premature but the constraint should be monitored for increasing theater_ratio (the signal that the constraint is persisting as performance after function atrophies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_authority_legitimacy,
    'Does the Magisterium''s claim to interpretive authority over AI governance derive from its theological teaching office, from demonstrated practical governance competence, or from some third source? And what grounds other parties'' acceptance of that authority?',
    'Empirical observation of who acknowledges Magisterium guidance in actual governance decisions (church leaders, governments, civil-society actors, tech companies). Normative interrogation of whether religious authority warrants secular-governance decisions. Analysis of alternative authority structures emerging in competing readings.',
    'If Magisterium''s authority is accepted only by Catholics and weakly by governments, the constraint''s enforceability depends entirely on civil-society mobilization and moral persuasion—suppression_requirement stays moderate. If secular governance structures actively incorporate Magisterium reasoning, the constraint''s reach expands and suppression requirements decrease (less active coercion needed because authority is recognized). If the authority is contested and fractured, the constraint risks splintering into multiple incompatible readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_authority_legitimacy, conceptual, 'The grounding and acceptance of Magisterium interpretive authority in secular tech governance.').

omega_variable(
    subsidiarity_vs_efficiency_operationalization,
    'How is the subsidiarity principle operationalized when it conflicts with efficiency? When worker participation or Global South autonomy slows technological deployment, which principle wins?',
    'Case studies of actual governance decisions where subsidiarity and efficiency collide: data-localization requirements that slow cloud deployment, participatory design processes that extend timelines, local-capability requirements that reduce centralized optimization. Document which principle governs and what trade-offs are visible in outcomes.',
    'If efficiency is sacrificed for subsidiarity, extractiveness remains moderate but resistance from tech monopolies increases and suppression_requirement rises. If efficiency is repeatedly privileged over subsidiarity, the constraint collapses into rhetoric and becomes piton-adjacent (theater_ratio rises). If subsidiarity is operationalized in genuinely constraining ways, the beneficiary groups (workers, Global South, vulnerable) experience real governance inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_vs_efficiency_operationalization, empirical, 'How subsidiarity principles are operationalized when they conflict with technological efficiency.').

omega_variable(
    reading_vs_sibling_stability,
    'Can this reading (magisterial subsidiarity) coexist with the technocratic-optimization reading in a single governance structure, or will they eventually foreclose each other? Can democratic pluralism broker between them, or do the readings represent incompatible authority structures?',
    'Observation of actual governance evolution: do jurisdictions attempt to blend magisterial, technocratic, and pluralist frameworks, and what breaks first? Normative analysis of whether subsidiarity and optimization can be harmonized or whether one must dominate. Empirical tracking of which reading gains institutional backing over the interval.',
    'If this reading and technocratic-optimization coexist without foreclosure, the constraint operates as tangled_rope (some extraction, some coordination) indefinitely. If they foreclose each other, the governance landscape bifurcates and extractiveness shifts sharply (either up if technocracy dominates, or down if this reading gains institutional backing). Democratic pluralism routing might create regulatory arbitrage where companies pick jurisdictions by preferred reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_stability, conceptual, 'The structural stability of this reading in a contested kernel environment with incompatible sibling readings.').

omega_variable(
    enforcement_mechanism_fragility,
    'The constraint is enforced primarily through moral suasion, civil-society pressure, and ecclesial witness rather than through state coercion or property-law enforcement. How stable is this enforcement against attrition when costs accumulate?',
    'Track enforcement mechanisms over time: do they strengthen (more coordinated civil society, more governments incorporating magisterial reasoning) or weaken (fatigue, regulatory capture, tech-company counter-mobilization)? Measure civil-society resource commitment and governmental accountability actions taken in the constraint''s name.',
    'If enforcement mechanisms strengthen, suppression_requirement can remain moderate (authority is recognized, so less overt coercion needed). If they attrit, the constraint risks shifting toward piton (theater increases, extraction persists, but active enforcement decays). The measurement trajectory (suppression_requirement flattening at t=15 onward) hints at equilibrium or early attrition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_fragility, empirical, 'The fragility of non-coercive enforcement mechanisms grounded in moral authority.').

omega_variable(
    universal_destination_goods_boundary,
    'The constraint derives from ''universal destination of goods'' doctrine, which asserts that AI-generated value should serve the common good rather than monopolistic extraction. But what counts as ''extraction'' vs. ''legitimate return on investment,'' and who decides that boundary?',
    'Doctrinal exegesis: trace how Catholic teaching distinguishes legitimate property and profit from unjust extraction. Empirical observation: document what income distribution or governance arrangements actual actors implement when claiming alignment with universal destination of goods. Case studies of disputes over whether a particular tech company''s profit model violates the principle.',
    'If the boundary is stable and recognizable, beneficiaries (workers, Global South, vulnerable) can appeal to clear principles when resisting extraction. If the boundary is blurry or contested within the Magisterium itself, the constraint loses operationality and risks collapse into theater. Tightening the boundary increases conflict with monopolies but clarifies what governance actually requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_destination_goods_boundary, conceptual, 'The operationality and stability of ''universal destination of goods'' as a boundary condition for legitimate AI governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This story is part of a four-constraint family decomposing the contested kernel 'ai_governance_legitimacy.' The magisterial-subsidiarity reading (this story) coexists with technocratic-optimization (tech-led, efficiency-focused), democratic-pluralist (deliberation-based, secular), and market-libertarian (exit-based, property-rights) readings. Each reading has its own ε, beneficiary/victim structure, and enforcement mode because each reading makes incompatible claims about what counts as legitimate technology governance. They are not perspectives on the same constraint; they are genuinely different constraints grounded in different authority structures and legitimacy criteria. Link direction: magisterial-subsidiarity influences technocratic-optimization (by establishing human-dignity as a non-negotiable constraint on efficiency) and coexists with democratic-pluralist and market-libertarian readings. The family composition allows the engine to model kernel contestation without forcing artificial harmonization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
