% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__incarnational_humanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__incarnational_humanism, []).

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
 *   constraint_id: ai_human_relationship__incarnational_humanism
 *   human_readable: AI Ordered to Integral Human Development (Incarnational Humanism Reading)
 *   domain: political_theology/technology_ethics/economic_justice
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel
 *   about the relationship between AI and human flourishing. The reading
 *   claims that AI must be ordered to integral human development — understood
 *   as growth in human capacity for relationship, dignity, vocation,
 *   participation, and solidarity — and that the human person remains
 *   irreducible to optimization. The reading arises from Catholic Social
 *   Teaching (especially post-Caritas in Veritate) and names a constraint:
 *   technology deployment must include communities in discernment, must serve
 *   the poor preferentially, must respect work as vocation, and must be
 *   limited by subsidiarity (decisions made at the most local competent
 *   level). The constraint is not purely coordination (communities do need
 *   some shared frame for technology governance) but substantially
 *   extractive: it distributes authority and decision-making power away from
 *   efficiency-maximizing firms toward intermediary institutions and
 *   communities — a transfer that firms resist, workers are often too weak to
 *   enforce, and states sometimes support rhetorically while abandoning in
 *   practice. The measurement trajectory shows extractiveness rising modestly
 *   (from 0.48 to 0.62 over 40 years) as technology deepens and firms develop
 *   more sophisticated workarounds; theater rising (0.35 to 0.48) as CSR
 *   rhetoric and 'ethical AI' theater absorb the constraint while real power
 *   concentrates; and suppression remaining high and stable (0.65-0.71)
 *   because the constraint's enforcement depends entirely on intermediary
 *   institutions maintaining their authority against firms' technical and
 *   capital advantages — a difficult maintenance requiring continuous energy.
 *
 * KEY AGENTS:
 *   - Communities of human flourishing (parishes, unions, cooperatives, neighborhood associations): claim authority to discern whether technologies serve integral development; benefit from constraint enforcement.
 *   - Workers in vulnerable sectors (manufacturing, agriculture, care, service): face automation decisions without participation; extracted from as their labor is optimized away.
 *   - Poor and marginalized: subject to algorithmic rationing and exclusion; locked in by poverty and identity frames; named as preferential beneficiary but often excluded from discernment.
 *   - Technology firms: agenda-setters for AI deployment; constrained by the requirement to subordinate to common good; maintain exit via regulatory arbitrage and rhetoric.
 *   - State regulators: hold formal authority to enforce but often defer to firms or adopt technocratic metrics.
 *   - Intermediary institutions (churches, unions, mutual aid): named as primary agents of technology discernment; constrained by resource asymmetries with firms.
 *   - Catholic doctrinal tradition: speaking authority carrying the reading's framework and legitimacy.
 *   - Secular technology ethics: share the reading's human-centered critique but operate from different grounding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__incarnational_humanism, 0.62).
domain_priors:suppression_score(ai_human_relationship__incarnational_humanism, 0.71).
domain_priors:theater_ratio(ai_human_relationship__incarnational_humanism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__incarnational_humanism, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_human_relationship__incarnational_humanism, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_human_relationship__incarnational_humanism, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__incarnational_humanism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_human_relationship__incarnational_humanism, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__incarnational_humanism, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__incarnational_humanism, "AI Ordered to Integral Human Development (Incarnational Humanism Reading)").
narrative_ontology:topic_domain(ai_human_relationship__incarnational_humanism, "political_theology/technology_ethics/economic_justice").

domain_priors:requires_active_enforcement(ai_human_relationship__incarnational_humanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__incarnational_humanism, '93d3b475-1531-4926-b915-2c6675f74dd3').
narrative_ontology:cs_kernel_codification('93d3b475-1531-4926-b915-2c6675f74dd3', formalized).
narrative_ontology:cs_authority_grounding('93d3b475-1531-4926-b915-2c6675f74dd3', lineage).
narrative_ontology:cs_interpretation_layer_present('93d3b475-1531-4926-b915-2c6675f74dd3').
narrative_ontology:cs_reading_relation('93d3b475-1531-4926-b915-2c6675f74dd3', ai_human_relationship__technocratic_optimization, forecloses).
narrative_ontology:cs_reading_relation('93d3b475-1531-4926-b915-2c6675f74dd3', ai_human_relationship__instrumental_subsidiarity, influences).
narrative_ontology:cs_axiom('93d3b475-1531-4926-b915-2c6675f74dd3', foundational, human_person_imago_dei_irreducible_optimization).
narrative_ontology:cs_axiom_status(human_person_imago_dei_irreducible_optimization, holdable).
narrative_ontology:cs_axiom_grounding('93d3b475-1531-4926-b915-2c6675f74dd3', human_person_imago_dei_irreducible_optimization, deontological).
narrative_ontology:cs_axiom('93d3b475-1531-4926-b915-2c6675f74dd3', foundational, technology_ordered_to_common_good_solidarity_preferential_option_poor).
narrative_ontology:cs_axiom_status(technology_ordered_to_common_good_solidarity_preferential_option_poor, holdable).
narrative_ontology:cs_axiom_grounding('93d3b475-1531-4926-b915-2c6675f74dd3', technology_ordered_to_common_good_solidarity_preferential_option_poor, instrumental).
narrative_ontology:cs_reference_frame('93d3b475-1531-4926-b915-2c6675f74dd3', human_person_as_imago_dei_irreducible_to_instrumental_logic).
narrative_ontology:cs_drift_state('93d3b475-1531-4926-b915-2c6675f74dd3', contemporary_market_efficiency_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93d3b475-1531-4926-b915-2c6675f74dd3', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__incarnational_humanism, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__incarnational_humanism, communities_of_human_flourishing).
narrative_ontology:constraint_beneficiary(ai_human_relationship__incarnational_humanism, workers_in_vulnerable_sectors).
narrative_ontology:constraint_beneficiary(ai_human_relationship__incarnational_humanism, poor_and_marginalized).
narrative_ontology:constraint_victim(ai_human_relationship__incarnational_humanism, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_human_relationship__incarnational_humanism, communities_unable_to_resist_technological_imposition).
narrative_ontology:constraint_victim(ai_human_relationship__incarnational_humanism, human_dignity_claims_undefended_in_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__incarnational_humanism, intermediary_institutions).
narrative_ontology:constraint_beneficiary(ai_human_relationship__incarnational_humanism, secular_technology_ethics_dissidents).
narrative_ontology:constraint_victim(ai_human_relationship__incarnational_humanism, workers_in_vulnerable_sectors).
narrative_ontology:constraint_victim(ai_human_relationship__incarnational_humanism, poor_and_marginalized).
narrative_ontology:constraint_vindicates(ai_human_relationship__incarnational_humanism, human_person_as_imago_dei).
narrative_ontology:constraint_vindicates(ai_human_relationship__incarnational_humanism, subsidiarity_as_empowerment).
narrative_ontology:constraint_vindicates(ai_human_relationship__incarnational_humanism, solidarity_as_transformative_interdependence).
narrative_ontology:constraint_vindicates(ai_human_relationship__incarnational_humanism, work_as_vocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Intermediary institutions (parishes, unions, mutual aid societies, cooperatives, local governance bodies) that claim the right to discern whether a technology serves their members' integral development — human capacity for relationship, work as vocation, participation in decisions that shape life. Under this reading, they are the primary agents of technological discernment, not passive consumers of imposed systems. They benefit from frameworks that recognize their authority to refuse technologies that degrade human relationships or dissolve their communities' cohesion.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, communities_of_human_flourishing, beneficiary,
    organized, generational, constrained, local).

% Face automation decisions made without their participation or consent. Their labor is evaluated by machines trained on efficiency metrics that do not account for work's role as vocation, dignity, or source of livelihood. Displacement into precarious gig work or service sectors represents a downward reclassification of their human status, not neutral technological progress. The constraint extracts from them via asymmetric decision-making: they bear the cost of being 'optimized away' while having no seat in the choice.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, workers_in_vulnerable_sectors, payer,
    powerless, biographical, trapped, global).

% Subject to algorithmic rationing, predictive policing, credit-denial systems, and health-access filtering that encode their marginalization into automation. Yet the preferential option for the poor asserts they have a claim on technological systems to serve them preferentially — not after efficiency is optimized. They are locked in by structural poverty (cannot opt out of systems that govern subsistence access) and by identity frames that tell them marginalization is natural or earned. A constraint that enforces their inclusion in design and benefit-sharing would serve them; current extraction operates by their exclusion.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, poor_and_marginalized, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__incarnational_humanism, poor_and_marginalized, beneficiary).

% Design, deploy, and defend AI systems evaluated primarily by efficiency, user engagement, and shareholder return. Under the incarnational reading they are subject to a constraint — subordination to common good — that limits their decision freedom. They have exit via regulatory arbitrage (moving operations to permissive jurisdictions) and via reframing (absorbing CSR language while retaining efficiency prioritization). The constraint requires them to demonstrate integral human development; they can often satisfy this with theater while extracting value from optimized systems.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, technology_firms, agenda_setter,
    institutional, biographical, mobile, global).

% Hold formal authority to enforce technological constraints on firms. Under incarnational humanism they are called to do so by subsidiarity (enforcing limits that preserve local communities' capacity to choose) and solidarity (defending the poor's preferential claim). In practice they often defer to firms, adopt technocratic metrics, or enforce only theater. Their constraint is to actually exercise the authority they claim — not as paternalistic imposition but as protection of communities' right to refuse.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, state_actors_regulatory, agenda_setter,
    institutional, generational, mobile, national).

% Churches, unions, mutual aid societies, cooperatives, neighborhood associations. The constraint names them as primary agents of technological discernment at subsidiary levels. They benefit materially if enforcement actually empowers them to guide technology deployment. They are constrained by asymmetric resources (firms have vastly greater technical and capital resources) and by pressures to adopt efficiency metrics to compete. The constraint is supposed to protect their authority; often it remains theater while real decisions concentrate in tech firms and regulators.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, intermediary_institutions, beneficiary,
    moderate, generational, constrained, regional).

% The living doctrinal tradition (encyclicals, bishops' statements, parish and diocesan statements) that articulates incarnational humanism. Carries the reading's authority and continuity. Not an agent in the sense of collecting rents but a speaking authority whose pronouncements carry weight in Catholic communities and in Catholic-influenced public discourse. Produces the framework for the reading's legitimacy.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, catholic_institutional_tradition, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_human_relationship__incarnational_humanism, catholic_institutional_tradition).

% Technologists, ethicists, policy advocates who argue for human-centered technology but from secular frameworks (capability approaches, human rights, feminist tech critique). They benefit from the constraint's enforcement because it legitimizes their critique; they are not locked into the Catholic theological frame. They can move between frameworks and jurisdictions. The constraint gives institutional backing to claims they make but from different grounding.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, secular_technology_ethics_dissidents, beneficiary,
    moderate, biographical, mobile, global).

% Workers in sectors without organized labor, without community institutions, without access to technical knowledge. They would benefit from the constraint most but are least able to exercise voice within its machinery. Their absence from discernment is a structural failure of the constraint: it is meant to empower them but often leaves them outside the conversation, visible only as statistics in automation studies.
narrative_ontology:constraint_stakeholder(ai_human_relationship__incarnational_humanism, displaced_workers_lacking_advocacy, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__incarnational_humanism, technology_firms).
narrative_ontology:fixing_cost_class(ai_human_relationship__incarnational_humanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes that technology deployment must be evaluated by whether it serves integral human development — human capacity for relationship, dignity, vocation, participation in communities — rather than efficiency or optimization alone. Coordinates multiple institutional actors (churches, unions, communities, regulators, firms) around a shared principle of technology subordination to human flourishing. Solves the coordination problem: 'How do we decide whether to deploy an AI system?' by asserting the decision must include those whose humanness is at stake, not just those who profit from optimization.
% TRANSFER_FUNCTION: Moves authority over technological discernment from efficiency metrics and shareholder logic to broader criteria: common good, integral human development, preferential option for the poor, work as vocation. Moves costs of technological displacement from individual workers onto society (through support, retraining, community protection) rather than concentrating them on those automated away. Transfers the frame of what technology is *for* — from individual productivity to human flourishing.
% ABSENT_VOICES: Displaced workers without organized labor or advocacy; workers in the Global South whose labor trains AI systems they will not benefit from; future generations who will inherit AI-shaped institutions; the poor themselves (often spoken for rather than speaking). The constraint names a preferential option for them but they are frequently absent from the forums where technology is actually decided. That absence is a known failure mode of the constraint — it stays theater when the excluded stay excluded.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement vanished, technology deployment would revert to efficiency maximization and shareholder return as primary criteria. Communities would lose their claim to refuse technologies; workers would face automation without collective voice; intermediary institutions would have no standing to shape technology. The world would not rearrange because incarnational humanism creates new value — it would rearrange because the existing arrangements presume no such constraint exists, and their restoration would be active.
% FOUNDING_PROBLEM: Industrial capitalism produced efficiency as the primary measure of technological worth, displacing human relationships, community cohesion, and work's meaning. Early AI amplifies this: systems optimized only for task performance encode the displacement into automation. The founding problem is the reduction of the human person to an input for optimization; technology that serves that reduction becomes a vector for dehumanization. The constraint arose to reassert the irreducibility of human persons and their communities to efficiency metrics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by displaced workers' testimony about automation without participation; by research on algorithmic bias and its effects on marginalized communities (Buolamwini, Gebru, O'Neil); by case studies of automation in manufacturing, agriculture, and care sectors showing outcomes that violate human dignity by the constraint's lights; by bishops' statements in industrial regions describing technological displacement of workers and erosion of community. NOT corroborated only by those who benefit from the constraint — the corroboration comes from those experiencing displacement and from independent researchers documenting harms.
narrative_ontology:disappearance_verdict(ai_human_relationship__incarnational_humanism, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__incarnational_humanism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__incarnational_humanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__incarnational_humanism, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__incarnational_humanism, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__incarnational_humanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__incarnational_humanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__incarnational_humanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the constraint transfers power away from markets and firms toward communities and intermediary institutions — a genuine transfer that involves costs. The constraint is not natural law (alternatives exist: pure market technology deployment, state technocracy, different ethical framings) so accessibility_collapse (0.45) is lower than a mountain — communities can refuse and do, but at significant cost (job loss, social pressure, exclusion from networks). Resistance is high (0.72) because technology workers, innovation advocates, and many communities actually prefer efficiency-first deployment; the constraint faces real resistance from those whose interests it constrains. Suppression is high (0.71) because enforcing this constraint requires sustained institutional presence and authority from churches, unions, and communities that lack firms' resources; if suppression is internalized, communities may give up even with legal space. Theater is moderate-high (0.48) because 'ethical AI' and CSR language absorb much of the constraint without actually shifting power — firms now run 'human-centered' design processes that still optimize for shareholder return; communities speak but decisions concentrate elsewhere. The claim is tangled rope (genuine coordination function — communities need shared frames for technology governance — AND asymmetric extraction: workers and poor bear the cost of being 'disarmed' from competitive optimization while firms extract efficiency gains). The metrics remain independent of the claim: if the engine computes a different type from the measured data, that divergence measures whether the reading's aspirational frame matches its structural reality.
 *
 * PERSPECTIVAL GAP:
 *   Technology firms perceive the constraint as pure extraction (loss of their right to optimize freely); they see the coordination function as rhetoric. Workers and poor perceive it as insufficient (if it were actually enforced they would have genuine voice; mostly it remains aspirational). Intermediary institutions perceive it as naming their proper role while also highlighting their resource constraints — they have authority without enforcement power. State regulators perceive it as politically impossible (firms are too powerful) and as threatening (intermediary institutions claiming authority over technology might later claim authority over other matters). The engine computes per-seat classification from these structural facts — the beneficiary seats (communities, intermediary institutions, workers if the constraint held) are partly trapped in constraints that limit their power, while the payer seat (firms) is institutionally powerful and mobile. Directionality diverges: from the firm's seat d approaches the target end (high extraction cost); from the community seat d depends on whether they actually get to exercise the authority the constraint names (if they do, d drops toward beneficiary; if theater dominates, d stays high for workers/poor).
 *
 * DIRECTIONALITY LOGIC:
 *   Workers_in_vulnerable_sectors: trapped (cannot exit automation except into worse-paid work), powerless globally, and subject to optimization without consent. Their directionality approaches 1.0 (full target). The constraint claims to serve them but enforcement is weak, so actual extraction persists. Beneficiaries (communities_of_human_flourishing) have directionality near 0.0 if the constraint is enforced (they gain authority and resources) but near 0.5 if theater dominates (they get a voice in forums with no real power). Poor_and_marginalized are identity-locked (poverty traps, discrimination frames) with powerless status and global reach — their directionality would be high target (1.0) but the constraint names them as preferential, so directionality may derive from the *intended* structural relationship (if enforced, near 0.0) rather than the actual one (if theater, near 0.8+). Intermediary_institutions are organized with moderate power but constrained exit (they cannot simply leave Catholic tradition or their communities) — directionality near 0.5 (symmetric costs and benefits if constraint is enforced; high cost and limited benefit if theater). Technology_firms are institutionally powerful with mobile exit (arbitrage to permissive jurisdictions) — directionality would be high target (1.0) under the constraint, but their mobility and resource advantage allow them to engineer effective exits so actual d may be lower (0.6-0.8 range). State_actors_regulatory have institutional power and some mobility; the constraint asks them to exercise authority they nominally hold, so directionality is near 0.5 (symmetric: they gain legitimacy if they enforce, lose authority if they defer).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is latent but not active in the current measured period (theater_ratio 0.35-0.48 is moderate; not yet in the 0.7+ zone where core function has clearly been replaced by performance). The founding problem (reduction of human persons to optimization; displacement of workers without consent; erasure of work's meaning) remains live — workers face automation without participation, algorithms exclude the poor, efficiency remains the dominant frame. But the constraint itself has acquired significant rhetorical adoption: firms now have Chief Ethics Officers, run responsible AI initiatives, create bias-detection tools. If this theater continues to rise while core function (actual community authority over technology deployment) remains weak, mandatrophy can develop. The risk is that the constraint becomes a legitimation mechanism: firms deploy harmful AI under the sign of 'integral human development'; communities get to speak in forums that have no binding power; workers face slightly more 'ethical' automation that still displaces them. The constraint would then serve to make extraction more durable by absorbing resistance into the management structure. Currently this is latent risk, not active mandatrophy. Enforcement remains possible if intermediary institutions build real power (union tech demands that win concrete victories, community refusals that stick, regulatory enforcement that actually blocks deployment). The mandatrophy trigger is if theater (0.48) rises above 0.65-0.70 while extractiveness stays stable and core coordination function (actual authority exercised by communities) remains weak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integral_human_development_operationalization,
    'What counts as ''integral human development'' in concrete technological design decisions? Is it operationalizable as a constraint, or does it require ongoing prudential judgment that resists quantification?',
    'Case studies of technology discernment by intermediary institutions (parish automation committees, union technology demands, cooperative AI governance pilots); analysis of whether these produce stable, repeatable criteria or require bespoke judgment in each case.',
    'If operationalizable as stable criteria, the constraint can be enforced mechanically and scaled. If it requires ongoing prudential judgment, enforcement depends entirely on the quality and independence of the judges — a tangled rope becomes vulnerable to capture when judges are corrupted or replaced. If it resists operationalization, the constraint may remain aspirational rather than binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integral_human_development_operationalization, conceptual, 'Whether integral human development can be specified as binding criteria or requires ongoing judgment.').

omega_variable(
    subsidiarity_as_empowerment_vs_paternalism,
    'When the constraint empowers intermediary institutions to refuse technology, are communities actually choosing for themselves, or are they choosing what the church/tradition has told them to choose? Is subsidiarity here empowerment or paternalism dressed as empowerment?',
    'Ethnographic study of technology discernment by communities claiming the constraint; analysis of whether refusals arise from community voice or from institutional authority; comparison with communities that face the same technology with different institutional backing.',
    'If choices are actually paternalistic, the constraint recreates a dominance structure (institutional authority over people''s choices) while claiming to serve human flourishing. The measured extraction would then include both the cost to communities of refusing valuable technologies AND the cost of being subordinate to institutional judgment. If choices are genuinely empowered, the constraint serves the reading''s intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_as_empowerment_vs_paternalism, empirical, 'Whether intermediary institutions empower or dominate the communities they speak for.').

omega_variable(
    preferential_option_for_poor_as_capture_vector,
    'Does naming the poor as preferential beneficiaries make the constraint MORE vulnerable to capture — i.e., does it invite elites to claim they serve the poor while actually extracting in the name of solidarity?',
    'Historical analysis of preferential-option rhetoric in Catholic institutions; comparison of rhetoric vs. actual resource distribution; case studies where the poor gained material benefit vs. cases where the poor remained excluded while institutions claimed solidarity.',
    'If the preferential option becomes a cover story, extraction actually rises because it now has a moral narrative. The measured extraction (0.62) might understate true extraction if the theater ratio rises. Conversely, if institutions actually redistribute to serve the poor, extraction falls and the constraint delivers its function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preferential_option_for_poor_as_capture_vector, empirical, 'Whether preferential option for the poor operates as genuine commitment or as moral cover for extraction.').

omega_variable(
    reading_vs_sibling_foreclosure_boundary,
    'Does the incarnational humanism reading''s assertion that ''human person is irreducible to optimization'' actually FORECLOSE the technocratic_optimization reading''s core premise? Or can both coexist as live positions in different institutional contexts?',
    'Logical analysis of the core premises: if one reading asserts humans are irreducible to optimization and the sibling asserts humans can be wholly understood as optimization candidates, can any single framework hold both? Or does embracing one require rejecting the other as structurally incoherent?',
    'If they foreclose each other, one reading must eventually eliminate the other — the constraint''s long-term success depends on institutionalizing the incarnational frame so thoroughly that technocratic optimization becomes incoherent within the constraint''s own system. If they coexist, they remain live competitors held by different institutional actors; the constraint becomes one voice in a permanent contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure_boundary, conceptual, 'Whether incarnational humanism logically forecloses technocratic optimization, or whether both remain live simultaneously.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (external barriers to deploying non-incarnational AI: law, funding limits, market access) or internalized (workers/communities have accepted that they are meant to be optimized and believe refusal is futile)?',
    'Post-exit suppression trajectory: if communities that successfully refuse one AI system then refuse the next (without need for external enforcement), suppression is internalized and portable. If suppression drops when external barriers are removed, it was structural.',
    'If internalized, the constraint''s effective suppression is higher than the scalar measure suggests — communities carry the suppression with them, limiting their capacity to drive alternatives even when given legal space. If structural, removing barriers (law, funding) could rapidly shift behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of non-incarnational AI deployment is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__incarnational_humanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__incarnational_humanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ai_h_tr_t0, observed).
narrative_ontology:measurement(ai_h_tr_t5, ai_human_relationship__incarnational_humanism, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(ai_h_tr_t5, observed).
narrative_ontology:measurement(ai_h_tr_t10, ai_human_relationship__incarnational_humanism, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(ai_h_tr_t10, observed).
narrative_ontology:measurement(ai_h_tr_t15, ai_human_relationship__incarnational_humanism, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(ai_h_tr_t15, observed).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__incarnational_humanism, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(ai_h_tr_t20, projected).
narrative_ontology:measurement(ai_h_tr_t30, ai_human_relationship__incarnational_humanism, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(ai_h_tr_t30, projected).
narrative_ontology:measurement(ai_h_tr_t40, ai_human_relationship__incarnational_humanism, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(ai_h_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__incarnational_humanism, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_h_be_t0, observed).
narrative_ontology:measurement(ai_h_be_t5, ai_human_relationship__incarnational_humanism, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(ai_h_be_t5, observed).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__incarnational_humanism, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(ai_h_be_t10, observed).
narrative_ontology:measurement(ai_h_be_t15, ai_human_relationship__incarnational_humanism, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(ai_h_be_t15, observed).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__incarnational_humanism, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(ai_h_be_t20, projected).
narrative_ontology:measurement(ai_h_be_t30, ai_human_relationship__incarnational_humanism, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(ai_h_be_t30, projected).
narrative_ontology:measurement(ai_h_be_t40, ai_human_relationship__incarnational_humanism, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(ai_h_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__incarnational_humanism, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(ai_h_su_t0, observed).
narrative_ontology:measurement(ai_h_su_t5, ai_human_relationship__incarnational_humanism, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(ai_h_su_t5, observed).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__incarnational_humanism, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(ai_h_su_t10, observed).
narrative_ontology:measurement(ai_h_su_t15, ai_human_relationship__incarnational_humanism, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_h_su_t15, observed).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__incarnational_humanism, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_h_su_t20, projected).
narrative_ontology:measurement(ai_h_su_t30, ai_human_relationship__incarnational_humanism, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(ai_h_su_t30, projected).
narrative_ontology:measurement(ai_h_su_t40, ai_human_relationship__incarnational_humanism, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(ai_h_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__incarnational_humanism, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__incarnational_humanism, 0.18).
narrative_ontology:affects_constraint(ai_human_relationship__incarnational_humanism, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__incarnational_humanism, ai_human_relationship__instrumental_subsidiarity).
narrative_ontology:affects_constraint(ai_human_relationship__incarnational_humanism, worker_automation_consent_regime).
narrative_ontology:affects_constraint(ai_human_relationship__incarnational_humanism, preferential_option_for_poor__structural).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest about AI's proper relationship to human persons and communities. The network links to sibling readings of the same kernel (technocratic_optimization, instrumental_subsidiarity) which have different structures, different beneficiaries, and different measured types. The three readings coexist as live institutional positions; progress in one reading does not eliminate the others, though it can shift relative institutional power. This constraint also affects downstream constraints about worker automation consent and the preferential option for the poor, because those constraints inherit the relational frame incarnational humanism establishes: if humans are irreducible to optimization, then automation requires consent; if the poor have a preferential claim, then technology must serve them preferentially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_human_relationship__incarnational_humanism, powerless, 0.85).
constraint_indexing:directionality_override(ai_human_relationship__incarnational_humanism, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
