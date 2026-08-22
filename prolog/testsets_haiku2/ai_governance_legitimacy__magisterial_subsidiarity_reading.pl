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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: AI Governance Legitimacy — Magisterial Subsidiarity Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel 'AI
 *   governance legitimacy.' The magisterial subsidiarity reading grounds
 *   legitimacy in Catholic Social Doctrine principles authoritatively
 *   interpreted by the Magisterium: common good, subsidiarity
 *   (decision-making near the affected), solidarity (preference for the
 *   vulnerable), and universal destination of goods (technology must serve
 *   all, not concentrated extraction). This reading entangles genuine
 *   coordination (aligning technology with human flourishing) with asymmetric
 *   extraction (redistributing power and surplus from monopolies and military
 *   institutions to workers and marginalized populations). The constraint
 *   lives through civil society coalition organizing, moral suasion from
 *   ecclesial witness, and nascent international law advocacy demanding
 *   participatory AI governance. The measuring period (0–25) captures rising
 *   Magisterium teaching intensity, civil society coalition growth, and early
 *   regulatory responses, with projected stabilization as the
 *   coordination/extraction entanglement reaches steady state. Key tension:
 *   the Magisterium's normative authority is itself contested; sibling
 *   readings deny its monopoly on legitimacy claims.
 *
 * KEY AGENTS:
 *   - Magisterium: sets normative frame, convenes legitimacy discourse, designates compliance structure
 *   - Global South workers: primary beneficiaries; trapped by algorithmic governance; voice articulated through Magisterium and civil society
 *   - Marginalized populations: vulnerable beneficiaries; face algorithmic bias and exclusion; protected under constraint's participatory mandate
 *   - Tech monopolies: primary victims of the constraint; face redistribution of decision-making and profit-sharing demands
 *   - Military-industrial complex: victim of constraint; faces reduced autonomy in AI weapons development
 *   - Civil society coalition: grassroots enforcement mechanism; translates principle to local governance; mobilizes moral and legal pressure
 *   - National governments: negotiate between constituent pressure, corporate pressure, and Catholic Social Doctrine teaching
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.48).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy — Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'dade761f-cd0d-4617-b7c4-10214a6004ae').
narrative_ontology:cs_kernel_codification('dade761f-cd0d-4617-b7c4-10214a6004ae', formalized).
narrative_ontology:cs_authority_grounding('dade761f-cd0d-4617-b7c4-10214a6004ae', lineage).
narrative_ontology:cs_interpretation_layer_present('dade761f-cd0d-4617-b7c4-10214a6004ae').
narrative_ontology:cs_reading_relation('dade761f-cd0d-4617-b7c4-10214a6004ae', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dade761f-cd0d-4617-b7c4-10214a6004ae', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_reading_relation('dade761f-cd0d-4617-b7c4-10214a6004ae', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('dade761f-cd0d-4617-b7c4-10214a6004ae', foundational, human_dignity_primacy_over_efficiency).
narrative_ontology:cs_axiom_status(human_dignity_primacy_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('dade761f-cd0d-4617-b7c4-10214a6004ae', human_dignity_primacy_over_efficiency, deontological).
narrative_ontology:cs_axiom('dade761f-cd0d-4617-b7c4-10214a6004ae', foundational, subsidiarity_requires_distributed_decision_authority).
narrative_ontology:cs_axiom_status(subsidiarity_requires_distributed_decision_authority, holdable).
narrative_ontology:cs_axiom_grounding('dade761f-cd0d-4617-b7c4-10214a6004ae', subsidiarity_requires_distributed_decision_authority, conventional).
narrative_ontology:cs_reference_frame('dade761f-cd0d-4617-b7c4-10214a6004ae', human_dignity_and_common_good_primacy).
narrative_ontology:cs_drift_state('dade761f-cd0d-4617-b7c4-10214a6004ae', contemporary_ai_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dade761f-cd0d-4617-b7c4-10214a6004ae', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, family_units).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, ecological_commons).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_organizations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, human_dignity_primacy).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church articulates normative principles for AI governance grounded in Catholic Social Doctrine. Issues authoritative guidance through encyclicals, papal statements, and Vatican offices; convenes international forums on technology ethics; legitimizes civil society governance frameworks through ecclesial witness. Does not directly enforce AI governance but designates what conformity to human dignity and common good means.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Labor in manufacturing (phone assembly, rare earth mining), data annotation, agricultural zones, and service sectors increasingly mediated by AI systems designed to minimize labor cost and maximize extraction. Subsidiarity principle demands decision-making authority remain close to them; solidarity demands their voice in governance structures that affect their livelihoods. The constraint, if enforced, would require transparency about algorithmic wage impacts, collective bargaining rights, and participatory design of automation. Without the constraint, they remain trapped in asymmetric algorithmic governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_workers, beneficiary,
    powerless, biographical, trapped, global).

% Face algorithmic bias and exclusion in credit scoring, hiring, criminal justice, benefits allocation, and healthcare. Identity-locked because the algorithmic category (creditworthiness, employability, recidivism risk) becomes fused with self-conception and life trajectory once assigned. Protection of the vulnerable is a foundational Catholic principle; the constraint demands participatory governance and transparency about algorithmic harm before systems are deployed at scale. They lack direct power to enforce this but are named as the moral subject the constraint exists to protect.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, biographical, identity_locked, global).

% Subsidiarity centers governance at the family and community level, not in distant algorithms owned by tech monopolies. Technology should support family formation, parental authority, educational autonomy, and local community decision-making. Instead, algorithmic systems are designed to extract behavioral data from families, target minors with attention manipulation, and subordinate family preferences to platform engagement metrics. The constraint demands technology be redesigned for family flourishing rather than data extraction.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, family_units, beneficiary,
    organized, generational, constrained, global).

% Face demands for radical transparency about algorithmic decision-making, training data provenance, and concentration of corporate power. Subsidiarity principle challenges monopoly centralization; solidarity principle challenges profit-maximization-at-any-cost business models. Compliance requires governance restructuring (worker representation on boards, participatory design processes), open-sourcing proprietary design documentation, revenue-sharing mechanisms with affected communities, and accountability to workers and marginalized populations. They bear the direct cost: redistribution of decision-making power and profit flows.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, tech_monopolies, payer,
    powerful, biographical, arbitrage, global).

% Uses AI for weapons systems targeting, autonomous military systems, surveillance infrastructure, and strategic dominance. The constraint demands subordination of military logic to human dignity and common good principles, which directly conflicts with efficiency-maximization and strategic victory rationales. Enforcement would require international treaties, civilian oversight mechanisms, and prohibition of certain AI applications in warfare and surveillance. They bear the constraint's cost as constraints on strategic capability and weapons development autonomy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Uses AI for algorithmic trading, credit scoring designed to extract maximum surplus from debtors, predatory lending automation, and financial surveillance. Universal destination of goods principle challenges extraction logic; common good principle challenges zero-sum optimization for shareholder returns. Compliance requires fiduciary duties reframed toward borrower welfare, transparent pricing algorithms, community benefit structures, and democratic oversight of algorithmic lending decisions. They bear costs as their rent-extraction mechanisms become subject to participatory and ethical oversight.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_sector, payer,
    powerful, biographical, arbitrage, global).

% Faith-based organizations, labor unions, environmental groups, indigenous communities, and development NGOs form the grassroots enforcement coalition. Translate Catholic Social Doctrine principles into local governance structures and accountability mechanisms; conduct participatory design processes; monitor compliance; mobilize moral and legal pressure on tech companies and governments. The constraint lives through their sustained organizing; without their work, Magisterium teaching would remain aspirational and never materialize into changed governance practices.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_organizations, agenda_setter).

% Negotiate between constituent pressure to enforce the constraint (through regulation, mandatory impact assessments, participatory governance requirements, international treaty participation) and corporate/military pressure to resist (on grounds of innovation sovereignty, competitiveness, national security). Subsidiarity principle demands they remain close to affected communities rather than cede governance to either concentrated private power (tech monopolies) or centralized international technocracy. They are the structural locus where the constraint is enforced or resisted.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments, payer).

% Computer scientists, AI ethicists, engineers, development practitioners, and policy researchers assess the feasibility and technical implications of participatory governance requirements, transparency demands, and algorithmic accountability. They serve as interpreters between principle and implementation: clarifying what subsidiarity means for algorithm design, what solidarity requires for data governance, what transparency looks like in proprietary systems. Their technical judgment shapes what compliance is credible without paralyzing innovation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, technical_experts, observer,
    institutional, biographical, mobile, global).

% Conservative Catholic factions, some evangelical and Protestant groups, and secular libertarian traditions resist the Magisterium's teaching on technology, viewing it as: ecclesiastical overreach into secular governance, excessive constraint on market liberty, subordination of innovation to ideology, illegitimate authority claim in pluralist societies. They would argue for narrower Church teaching, religious pluralism in governance frameworks, and property-rights-based approaches to technology. They are excluded from this reading's deliberative space.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, ecclesiastical_opponents, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_organizations).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns AI development, deployment, and governance with human dignity, common good, subsidiarity (decision-making authority near those affected), and solidarity (preference for protection of the vulnerable). Solves the coordination problem of preventing AI from becoming a tool of monopoly extraction, algorithmic exploitation, military domination, or predatory finance by centering beneficiary participation, transparent accountability to affected communities, and subordination of technology to human purposes rather than vice versa.
% TRANSFER_FUNCTION: Transfers decision-making authority from concentrated private corporate and state-military control to distributed community, worker, and marginalized-population governance structures; transfers a share of AI-generated economic surplus to affected workers and communities through profit-sharing mechanisms, reparative justice for historical harms, and community benefit requirements; transfers technological knowledge from proprietary secrecy to participatory audit and open design documentation.
% ABSENT_VOICES: Market libertarians argue the constraint illegitimately coerces through redistributive mechanisms and fails to recognize voluntary exchange and property rights as foundational. Technocratic optimization advocates argue efficiency and innovation cannot be subordinated to ethical principles without stifling beneficial progress. Democratic pluralists argue the Magisterium should not claim monopoly authority over governance principles in secular pluralist societies. Populations in extractive-dependent economies (mining regions, labor arbitrage zones, speculative finance hubs) who benefit from concentration of tech power and minimal regulation are excluded from representation.
% DISAPPEARANCE_RATIONALE: If the constraint and its enforcement vanished entirely, AI governance would consolidate further in corporate profit-maximization and military-strategic logic unchecked by human dignity principles. Worker participation in algorithmic governance would collapse; marginalized populations' voices in system design would evaporate; tech monopolies would face zero accountability to common good; extractive finance would accelerate with no ethical oversight; subsidiarity principle would be abandoned in favor of centralized corporate control. The world rearranges because the constraint structures who has decision-making voice; removing it restores concentrated private and state power over AI governance.
% FOUNDING_PROBLEM: Technology in modernity tends toward concentration of power in private corporations and military institutions, bypassing democratic deliberation and sacrificing human dignity to profit extraction and strategic dominance. Catholic Social Doctrine, rooted in natural law and incarnational theology, teaches that technology must be subordinated to the common good and must protect the vulnerable. The founding problem is: how does humanity ensure that AI systems serve life, community, and human dignity rather than concentrating power, extracting surplus, and dominating the weak?
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium attests the problem is live and urgent (Caritas in Veritate 2009, Laudato Si' 2015, ongoing papal addresses on technology, Vatican statements on AI ethics). Independent corroboration comes from: labor economists documenting algorithmic wage suppression and worker surveillance; computer scientists describing power concentration in 5–10 AI-leading corporations; international development researchers showing tech-enabled extraction and labor arbitrage in Global South; military analysts documenting proliferation of autonomous weapons; civil society organizations (labor unions, environmental groups, development NGOs, indigenous communities) attest that tech-mediated governance directly shapes their organizing and that Catholic Social Doctrine principles, if enforced, would shift power relations in their favor. Opposition comes from the tech sector, military-industrial complex, and extractive finance, who do not dispute the problem's existence but deny the Magisterium's authority to set governance terms and argue that efficiency and security must take priority over Catholic principles.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.48) is moderate-high because the constraint requires significant redistribution of power and surplus from the concentrated beneficiaries (tech, military, finance) to the dispersed beneficiaries (workers, marginalized, families). It is not maximum extraction (0.7+) because the Magisterium's authority is institutional but not coercive — it relies on moral suasion and civil society mobilization, not state monopoly violence. Suppression (0.52) rises over the interval because the constraint, as it gains institutional footing, faces active resistance from the tech and military sectors trying to maintain their extractive logic — the suppression measures the active counter-enforcement those sectors deploy to resist the constraint's realization. Theater ratio (0.28) is low-moderate because the constraint's coordination function (aligning technology with human dignity) is genuine and coherent; most activity is functional (participatory governance design, transparency mechanisms, impact assessment, community accountability) rather than performative. However, theater rises modestly (0.15 to 0.28) because as civil society institutionalizes the constraint, bureaucratic compliance theater (certification, auditing, best-practice rhetoric without substance) emerges — the constraint risks becoming a legitimizing cover for marginally-reformed extraction. The measurement series are one shared time grid: every metric is measured or projected at every time point (0, 5, 10, 15, 20, 25), enabling temporal drift analysis.
 *
 * PERSPECTIVAL GAP:
 *   The tech monopoly seat and the Magisterium seat experience this constraint radically differently. From the monopoly's perspective, the constraint is illegitimate overreach — it subordinates efficiency and shareholder returns to principles they reject, and it redistributes their extracted surplus to communities they exclude from their calculations. From the Magisterium's seat, the constraint is a moral imperative grounded in natural law and the incarnational dignity of workers and vulnerable populations; the tech sector's resistance to transparency and participatory governance is a refusal of human dignity. The engine computes this divergence: the monopoly seat's directionality will sit near 1.0 (full target), while the Magisterium seat's d will sit near 0.0 (structural beneficiary of moral authority legitimation). This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (global_south_workers, marginalized_populations, family_units, ecological_commons) get d close to 0.0 because the constraint's operation directly benefits them: it subordinates technology to their dignity, gives them participatory voice in algorithmic governance, and protects them from concentrated extraction. Their exit options are trapped or identity_locked; they depend on the constraint's enforcement. Tech monopolies and the military-industrial complex get d close to 1.0 because they are the targets: the constraint extracts from their concentrated power, requires transparency they resist, and redistributes their surplus to beneficiaries they currently ignore. Extractive finance sits similarly (d ~ 0.95). Civil society organizations sit near d ~ 0.2 because they benefit from the constraint's framing (it legitimizes their organizing) but also bear enforcement costs (they must continually mobilize and translate principle to practice). National governments sit near d ~ 0.5 because they are caught between constituent pressure to enforce and corporate/military pressure to resist. Technical experts sit near d ~ 0.4 because the constraint gives their expertise new institutional weight (design for human dignity, participatory governance), but it also constrains their work (no longer pure optimization logic).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because its founding problem (how to subordinate technology to human dignity) remains live and urgent; the Magisterium's teaching on this problem is actively developing, not ossified; and the civil society coalition is organized and growing. The constraint IS tangled rope and not piton because it entangles genuine coordination (aligning tech with human flourishing) with real asymmetric extraction (redistributing power from monopolies to workers). The constraint is not pure rope because the beneficiaries (workers, marginalized populations) cannot unilaterally enforce it — they depend on Magisterium legitimacy and national government coercion to make it stick. The constraint is not pure snare because the coordination function is real and beneficiaries genuinely benefit, not merely manipulated. The Tangled Rope classification captures the entanglement: the coordination requires enforcement (suppression of tech resistance, military obstruction), and the enforcement is justified only by the coordination's necessity (dignity, subsidiarity, solidarity are not arbitrary demands).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_authority_contested,
    'By what authority does the Magisterium claim to set norms for AI governance across secular pluralist societies where many reject Catholic teaching?',
    'Legitimacy testing: does the Magisterium''s teaching gain practical influence through moral persuasion and civil society adoption, or does it require state power to enforce? Where enforcement requires coercion beyond moral suasion, the authority claim is contested.',
    'If legitimacy derives from moral persuasion alone (civil society coalition adoption), the constraint is tangled_rope with lower suppression requirement because beneficiaries voluntarily organize. If it requires state coercion, suppression climbs and the constraint risks becoming snare (extraction justified by controversial religious authority). If it remains purely aspirational, the constraint becomes piton (moral theater without institutional teeth).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_authority_contested, conceptual, 'Whether Magisterium teaching on AI governance possesses legitimate authority in secular pluralist contexts.').

omega_variable(
    subsidiarity_extraction_tension,
    'Can the constraint coherently demand both subsidiarity (decision-making close to affected communities) AND redistributive extraction (requiring tech monopolies to share profits globally)? Does global redistribution violate local subsidiarity?',
    'Praxis test: do communities that gain redistributed resources experience decision-making as subsidiarity-respecting (their voice shapes how resources are used) or as paternalistic imposition (external actors decide for them)? Does civil society coalition governance structure reflect subsidiarity or hierarchical distribution?',
    'If subsidiarity is violated by global redistribution, the constraint''s internal coherence fractures; subsidiarity and solidarity may be in tension rather than complementary. If community voice genuinely shapes resource use, the tension resolves. The classification would shift from tangled_rope toward snare if subsidiarity is sacrificed to solidarity''s redistributive demands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_extraction_tension, conceptual, 'Whether subsidiarity and global solidarity demands are structurally compatible.').

omega_variable(
    tech_monopoly_reform_capacity,
    'Can tech monopolies genuinely comply with participatory governance and transparency demands, or are these demands structurally incompatible with monopoly business models?',
    'Natural experiment: jurisdictions (EU Digital Markets Act, proposed regulations) that mandate transparency and participatory governance; measurement of whether monopolies adapt or relocate/resist. If adaptation occurs without fundamental business model change, reform is possible. If monopolies exit jurisdictions or collapse under compliance, the demands are structurally incompatible.',
    'If reform is possible, the constraint is tangled_rope (coordination and extraction entangled but both achievable). If demands are structurally incompatible, the constraint becomes snare (extraction disguised as coordination; monopolies must either comply (exit market) or resist (face legal/moral sanctions)). Refusal to reform becomes evidence of bad faith.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tech_monopoly_reform_capacity, empirical, 'Whether tech monopoly structures can accommodate participatory governance and transparency.').

omega_variable(
    sibling_readings_foreclosure,
    'Does the magisterial subsidiarity reading logically foreclose the market libertarian reading, the technocratic optimization reading, and the democratic pluralist reading? Or do they coexist as genuinely different frameworks held by different parties?',
    'Logical analysis: examine the core premises of each reading. If the magisterial reading''s premises (human dignity primacy, subsidiarity as non-negotiable) logically entail the rejection of market libertarianism''s core premise (efficiency and profit maximization as primary), then foreclosure is structural. If both can be held by different parties without internal contradiction within each party''s framework, they coexist.',
    'If foreclosure is structural, the constraint''s enforcement would require suppression of market libertarian and technocratic viewpoints as incoherent. If coexistence is possible, the constraint lives in a pluralist society where different sectors defend different readings. The classification of suppression (coercion to enforce the reading vs. suppression of incoherent alternatives) differs radically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_readings_foreclosure, conceptual, 'Whether sibling readings logically foreclose the magisterial reading or coexist as live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ai_g_tr_t0, observed).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(ai_g_tr_t5, observed).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(ai_g_tr_t10, observed).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(ai_g_tr_t15, observed).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(ai_g_tr_t20, projected).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(ai_g_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement_basis(ai_g_be_t0, observed).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(ai_g_be_t5, observed).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(ai_g_be_t10, observed).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(ai_g_be_t15, observed).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(ai_g_be_t20, projected).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(ai_g_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(ai_g_su_t0, observed).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(ai_g_su_t5, observed).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(ai_g_su_t10, observed).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(ai_g_su_t15, observed).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(ai_g_su_t20, projected).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(ai_g_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the AI governance legitimacy kernel family. Four structurally distinct readings decompose the natural-language claim 'AI governance should conform to Catholic Social Doctrine' into four separate constraint stories, each with its own ε, beneficiary/victim structure, and type classification. This reading (magisterial_subsidiarity) foreclose-coexists-or-influences the sibling readings depending on whether the parties holding each reading occupy the same decision-making seat or different seats. The family structure enables comparative analysis of how the same contested kernel produces radically different constraint classifications depending on which reading is instantiated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
