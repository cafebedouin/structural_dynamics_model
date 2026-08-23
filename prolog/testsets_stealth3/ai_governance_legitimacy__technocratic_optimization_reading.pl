% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__technocratic_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__technocratic_optimization_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__technocratic_optimization_reading
 *   human_readable: Technocratic Optimization Reading of AI Governance Legitimacy
 *   domain: theological/technological/political
 *
 * SUMMARY:
 *   An encyclical on artificial intelligence has made the legitimacy of AI
 *   governance a contested theological-political question, and this story
 *   authors one reading of that contest: the
 *   technocratic_optimization_reading, on which AI governance is legitimate
 *   insofar as it maximizes aggregate welfare, efficiency, and innovation,
 *   with the encyclical's principles admitted only as aspirational values to
 *   be balanced against feasibility and growth imperatives. The standing
 *   arrangement this story measures is a governance regime in which expert
 *   bodies define evaluation benchmarks and safety standards, captured
 *   regulators certify conformity, and market competition disciplines
 *   deviation: a real coordination achievement wrapped around an asymmetric
 *   cost structure, since the gains of rapid deployment accrue to firms,
 *   investors, skilled labor, and early adopters while displacement, digital
 *   exclusion, and opaque profiling land on people the aggregate metrics
 *   barely register. The UKE_SCOPE manifest hypothesis was rope; analysis
 *   refined the claim to tangled_rope because the declared victim surface
 *   plus actively maintained enforcement (expert gatekeeping, regulatory
 *   capture, competitive discipline) satisfies the hybrid signature — genuine
 *   coordination and asymmetric extraction operating through the same
 *   structure. Per the claim/metric independence rule, the claimed type and
 *   the metric scores below are authored independently; the engine computes
 *   per-seat classifications from the structural data, and any divergence is
 *   the measurement the corpus exists to take. KEY AGENTS (by structural
 *   relationship): - technocratic_standards_bodies: agenda-setter
 *   (institutional/constrained) — administers benchmarks, certification, and
 *   advisory consensus; collects authority from the regime's centrality -
 *   ai_development_firms: primary beneficiary and co-agenda-setter
 *   (institutional/arbitrage) — captures deployment gains, co-authors the
 *   frameworks it is evaluated under - venture_capital_investors: beneficiary
 *   (powerful/arbitrage) — funds the build-out under a return profile the
 *   frame protects - high_skill_technical_labor: beneficiary
 *   (organized/mobile) — scarce skills priced by the build-out -
 *   early_adopter_consumers: incidental beneficiary (moderate/mobile) — early
 *   capability at subsidized prices - displaced_workers: primary target
 *   (powerless/constrained) — automation losses register only as transition
 *   costs - digitally_excluded_communities: structural target
 *   (powerless/trapped) — AI-mediated defaults deepen an infrastructure gap -
 *   algorithmically_profiled_individuals: diffuse target (powerless/trapped)
 *   — scored by systems they cannot contest - national_ai_regulators:
 *   captured administrator (institutional/constrained) -
 *   displaced_sector_unions: excluded voice (organized/trapped) -
 *   automated_decision_subject_advocates: excluded voice (organized/trapped)
 *   - technology_ethics_scholars: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.38).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.6).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "Technocratic Optimization Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological/technological/political").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, '2eeb35c3-83b6-42ba-9cf1-260e66d88205').
narrative_ontology:cs_kernel_codification('2eeb35c3-83b6-42ba-9cf1-260e66d88205', distributed).
narrative_ontology:cs_authority_grounding('2eeb35c3-83b6-42ba-9cf1-260e66d88205', expertise).
narrative_ontology:cs_interpretation_layer_present('2eeb35c3-83b6-42ba-9cf1-260e66d88205').
narrative_ontology:cs_reading_relation('2eeb35c3-83b6-42ba-9cf1-260e66d88205', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('2eeb35c3-83b6-42ba-9cf1-260e66d88205', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eeb35c3-83b6-42ba-9cf1-260e66d88205', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('2eeb35c3-83b6-42ba-9cf1-260e66d88205', foundational, legitimacy_from_optimization_performance).
narrative_ontology:cs_axiom_status(legitimacy_from_optimization_performance, holdable).
narrative_ontology:cs_axiom_grounding('2eeb35c3-83b6-42ba-9cf1-260e66d88205', legitimacy_from_optimization_performance, instrumental).
narrative_ontology:cs_axiom('2eeb35c3-83b6-42ba-9cf1-260e66d88205', foundational, dignity_as_secondary_optimization_parameter).
narrative_ontology:cs_axiom_status(dignity_as_secondary_optimization_parameter, holdable).
narrative_ontology:cs_axiom_grounding('2eeb35c3-83b6-42ba-9cf1-260e66d88205', dignity_as_secondary_optimization_parameter, empirically_contingent).
narrative_ontology:cs_reference_frame('2eeb35c3-83b6-42ba-9cf1-260e66d88205', expert_performance_legitimacy_framework).
narrative_ontology:cs_drift_state('2eeb35c3-83b6-42ba-9cf1-260e66d88205', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2eeb35c3-83b6-42ba-9cf1-260e66d88205', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, ai_development_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_technical_labor).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopter_consumers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, technocratic_standards_bodies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, early_adopter_consumers).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, aggregate_welfare_maximization).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, technocratic_expertise_legitimacy).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, innovation_as_progress_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff and run the expert bodies that define evaluation benchmarks, certify safety cases, and advise governments on AI deployment. Their authority, funding, and relevance depend on the optimization frame remaining the operative standard for governance questions. Exit would mean leaving the field where their expertise is priced; they can move between institutions but not out of the expert-consensus system.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technocratic_standards_bodies, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, technocratic_standards_bodies, beneficiary).

% Build and deploy frontier AI systems. They capture the revenue and capability gains of rapid deployment while the governance frame treats displacement, exclusion, and profiling harms as trade-offs to be managed. Leading labs co-author governance frameworks, staff advisory bodies, and shape the standards they are evaluated against; they can relocate operations or restructure products across jurisdictions when any single regime tightens.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, ai_development_firms, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, ai_development_firms, agenda_setter).

% Fund AI startups and infrastructure on theses that assume fast deployment and low ethical overhead. The optimization frame protects their return profile by keeping compliance costs predictable and low. Capital moves freely across borders and sectors; exposure to any one jurisdiction's rules is a portfolio decision.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, venture_capital_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% The researchers, engineers, and product staff whose scarce skills command high wages in the AI build-out. They benefit from the sector's expansion and from employer-provided purpose narratives, and they can move between employers and countries at will. Their main exposure is skill obsolescence if the capability frontier turns, and many carry unease about downstream harms they do not personally decide.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_technical_labor, beneficiary,
    organized, biographical, mobile, global).

% Users who take up new AI products first, getting capability and convenience at prices subsidized by growth-stage capital. They pay with data, attention, and exposure to manipulation and error the products have not yet worked out. Individual exit is easy — stop using the product — but the products diffuse into services they cannot avoid.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopter_consumers, beneficiary,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, early_adopter_consumers, payer).

% Workers in translation, customer service, illustration, driving, and clerical functions whose tasks are automated under deployment decisions justified by aggregate productivity gains. Retraining programs exist but are underfunded relative to the pace of displacement; age, geography, and family ties limit relocation. Their losses register in the aggregate metrics only as transition costs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, biographical, constrained, national).

% Communities without reliable connectivity, devices, or digital literacy — concentrated in poorer regions and older populations — for whom AI-mediated banking, benefits, and healthcare triage become the default. Exclusion compounds: children without access fall further behind each year. There is no exit from an infrastructure gap; the alternative to inclusion is deepening exclusion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities, payer,
    powerless, generational, trapped, global).

% People scored, ranked, or flagged by automated systems in credit, hiring, policing, welfare eligibility, and immigration. They typically learn of the profile only through its consequences — a declined application, a flagged account — and contest channels are slow, costly, and rarely effective. They cannot opt out of systems embedded in the institutions they must deal with.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_individuals, payer,
    powerless, biographical, trapped, national).

% Agencies charged with AI oversight inside the optimization frame: they certify conformity, run sandboxes, and publish guidance. Rulemaking depends on industry technical expertise, so their standards track what firms can build; they gain mandate and budget as AI expands but absorb public blame when optimization harms surface. Moving against the frame would cost them their expert base and inter-agency standing.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, national_ai_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Unions and worker centers in the sectors most exposed to automation. They hold bargaining relationships and concrete policy proposals — severance floors, transition funds, deployment notice requirements — but no seat in the expert bodies where deployment standards are set. Their input enters, when at all, as consultation submissions answered after decisions are made.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_sector_unions, excluded,
    organized, biographical, trapped, national).

% Civil-society organizations working on due process, anti-discrimination, and welfare rights for people subject to automated decisions. They would demand contestability rights, impact assessments with affected-community standing, and limits on high-stakes opaque scoring. Expert-led venues treat these as implementation details to be optimized rather than claims to be adjudicated, and the advocates lack the technical credentials the venues require for standing.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, automated_decision_subject_advocates, excluded,
    organized, biographical, trapped, national).

% Academic researchers in science-and-technology studies, political theology, and AI ethics who document how the regime distributes gains and burdens and how its legitimacy claims hold up. They publish, advise, and testify but hold no enforcement power; their analyses circulate among the same expert bodies whose frame they examine.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technology_ethics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__technocratic_optimization_reading, ai_development_firms).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__technocratic_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of governing a rapidly evolving general-purpose technology: a shared optimization metric framework gives firms, investors, and regulators a common standard for evaluating deployment, comparable safety benchmarks, and a predictable cross-jurisdiction environment — without which governance fragments into incommensurable value claims.
% TRANSFER_FUNCTION: Moves the costs of optimization — job displacement, infrastructural exclusion, opaque profiling harms, externalized risk — from the firms and investors who capture the gains onto workers, excluded communities, and decision subjects whose losses are absorbed as acceptable trade-offs in aggregate metrics; and moves authority over AI governance questions from democratic and religious deliberative venues to expert bodies.
% ABSENT_VOICES: Displaced-sector unions and advocates for people subject to automated decisions would object that the regime counts their harms only as parameters, not as claims; they hold no seat in the expert venues where deployment standards are set. Religious social-doctrine bodies are admitted only as one more input to be balanced, never as an authoritative voice. The apparent unanimity of expert consensus is partly an artifact of who was in the room.
% DISAPPEARANCE_RATIONALE: Deployment pipelines, investment theses, standards bodies, and regulatory frameworks are organized around the optimization-legitimacy criterion; overnight removal would force every jurisdiction and firm to re-found governance on some other legitimacy basis — democratic consent, doctrinal conformity, or property rights — repricing investment and stalling or fragmenting deployment until new frameworks stabilized.
% FOUNDING_PROBLEM: Early AI governance faced a genuine coordination vacuum: no shared standards for safety evaluation, no common basis for cross-jurisdiction deployment decisions, and a real risk that fragmented, precaution-heavy regulation would stall beneficial applications. The technocratic reading was built to fill that vacuum — let expertise and demonstrated performance, not value pluralism, set the terms.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by intergovernmental bodies (OECD, UN) documenting real governance fragmentation, and by insurance and procurement sectors demanding comparable safety benchmarks. Notably, the encyclical tradition itself attests the coordination problem is live while disputing that optimization should hold interpretive monopoly; displaced-worker organizations dispute the weighting, not the existence, of the coordination problem.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__technocratic_optimization_reading_tests).
:- end_tests(ai_governance_legitimacy__technocratic_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored independently of the claim. Extractiveness 0.38: the referent is the standing arrangement under contest, assessed by this reading's own lights — the reading concedes real costs (displacement, exclusion, profiling) while holding that aggregate gains dominate, so the moderate value is its honest self-assessment, not an endorsement, and it sits inside the manifest's 0.30–0.40 band. Suppression 0.60: enforcement runs through expert gatekeeping (dignity-based arguments are structurally inadmissible in venues that require metric form), regulatory capture (standards track what firms can build), and competitive discipline (firms that internalize ethical costs lose to those that do not) — coercive in effect without legal coercion; it is authored as a raw structural property and is NOT scaled by power or scope, only extractiveness is. Theater 0.40: core optimization coordination is functional; a growing share of the ethical-governance layer is performative. Accessibility_collapse 0.45: alternatives do not fully collapse — the three sibling readings remain institutionally live (doctrinal bodies, democratic legislatures, libertarian policy shops), which is precisely what keeps this a tangled rope rather than a snare. Resistance 0.60: labor organizing, digital-rights litigation, the encyclical tradition, and democratic legislative pushback all contest the frame. All three measurement series share one time grid (points 0–24 at intervals of 4) so the engine samples a complete row at every point; suppression_requirement is authored because the story specifically tracks enforcement-machinery build-up over the interval — from ad-hoc expert advice to institutionalized safety institutes and captured conformity regimes — not because suppression varies idly.
 *
 * PERSPECTIVAL GAP:
 *   From the standards-body seat the arrangement is the only workable governance form for a general-purpose technology: metrics make claims commensurable, and the sibling readings are sentiment without an evaluation protocol. From the displaced-worker and profiled-individual seats the same structure counts their losses as parameters — the balance operation resolves against them because they are not in the room where weights are set. The inter-institutional gap runs between firms (arbitrage exit — any single jurisdiction's tightening is a portfolio decision) and national regulators (constrained exit — their expertise, mandate, and standing are inside the frame), so two nominally institutional actors experience opposite pressures from the same rules. The same-level gap among victims: displaced workers are individually powerless but latently coalition-capable through unions and sectoral organization, while profiled individuals are diffuse and hard to organize — a coalition of displaced workers, unions, and decision-subject advocates is the main structural threat to the frame from below, and its absence is partly an artifact of venue design.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain reads the beneficiary/victim declarations and exit atoms. ai_development_firms and venture_capital_investors sit nearest the beneficiary end: declared beneficiaries with arbitrage-grade exit, so effective extraction inverts toward subsidy for them. high_skill_technical_labor is a beneficiary with mobile exit — low d, with a small upward pull from obsolescence exposure the derivation does not see. early_adopter_consumers are dual-positioned (benefit now, pay in data and exposure) and derive near-symmetric. The victim set derives high d: displaced_workers (powerless, constrained), digitally_excluded_communities (powerless, trapped — nearest the full-target end), algorithmically_profiled_individuals (powerless, trapped). technocratic_standards_bodies carry a dual declaration (agenda_setter + beneficiary): they administer and collect authority, deriving low-mid d. national_ai_regulators are agenda-setters with no beneficiary declaration; their derived d falls to the canonical fallback, which understates their capture — this commentary flags the capture gradient rather than authoring a directionality override, because an override is keyed to the power atom and a single 'institutional' correction would also distort the firms and standards bodies sharing that atom. Scope: the regime operates at global scope with national verification capacity, which scales effective extraction upward for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a governance vacuum for a fast-moving general-purpose technology — is still live, so this is not a mandatrophy case: the mandate has not outlived its function, and the classification should not read atrophy into a regime that is actively maintained. The mandatrophy-relevant risk is different: decay of the balancing function. The reading promises that the encyclical's principles are 'balanced against feasibility'; the theater series shows the performative share of ethical-governance activity rising from 0.15 to 0.40 across the interval — ethics boards, principles documents, and impact templates that document rather than decide. If that trajectory continues, the balance function decays into ritual while the optimization core persists, and the constraint drifts toward the snare side of the tangled-rope band without any single decision point. The tangled_rope classification is what keeps both failure modes visible: a rope reading (the manifest hypothesis) would hide the victims behind the coordination benefit; a snare reading would erase the genuine coordination benefit that makes reform, rather than abolition, the structurally available remedy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the technocratic_optimization_reading of the ai_governance_legitimacy kernel — one of four live readings whose disagreement is located in (a) the seat of final adjudicative authority over AI governance (expert performance vs. magisterial interpretation vs. democratic consent vs. property rights) and (b) the status of the encyclical''s principles (authoritatively binding vs. balanceable parameters vs. one voice among many vs. partially coercive). Would instantiating a sibling reading restructure the beneficiary/victim surface and recompute the classification?',
    'Author and compile the three sibling reading stories against the same standing arrangement; compare computed per-seat classifications, epsilon, and victim sets across the kernel family.',
    'Under the magisterial reading the victim set expands (any optimization that violates doctrinal priority counts as a burden) and epsilon rises; under the market_libertarian reading the victim set contracts and the mandates themselves become the enforcement object; under the democratic reading extraction is re-indexed onto the consent deficit rather than the cost distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a four-reading kernel; all values are reading-indexed over a shared referent.').

omega_variable(
    aggregate_metric_visibility,
    'Do the regime''s aggregate welfare, efficiency, and innovation metrics systematically undercount the harms borne by displaced workers, digitally excluded communities, and algorithmically profiled individuals — and by how much?',
    'Distributional audits of AI deployment: longitudinal displacement studies with counterfactual reemployment data; infrastructure-access panels for excluded regions; algorithmic-impact assessments with affected-population standing and published error distributions.',
    'A large undercount means epsilon is understated even by this reading''s own lights and the constraint drifts toward the snare side of its band; if the metrics capture most harms, the coordination framing holds and the moderate value stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_metric_visibility, empirical, 'Whether the optimization metric framework renders victim costs structurally invisible.').

omega_variable(
    capture_gradient_in_expert_bodies,
    'How independent of the benefiting firms are the expert bodies whose consensus enforces the optimization frame — the standards institutes, safety evaluators, and advisory councils?',
    'Revolving-door and funding-disclosure analysis of standards and safety bodies; systematic comparison of expert-body recommendations against firm positions; audit of who drafts conformity standards.',
    'High capture raises suppression and epsilon (the enforcement layer serves the beneficiaries, not the coordination function); low capture supports the genuine-coordination component of the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_gradient_in_expert_bodies, empirical, 'Independence of the expert enforcement layer from the regime''s beneficiaries.').

omega_variable(
    foreclosure_vs_coexistence_authority_claims,
    'Is the relation between this reading and the magisterial_subsidiarity_reading a genuine logical foreclosure (no single framework can hold both authority claims over the encyclical''s principles), or could a hybrid framework — doctrine-constrained optimization — instantiate both partially, downgrading the relation to coexists_with?',
    'Conceptual analysis of whether one governance framework can assign final adjudicative authority to both demonstrated performance and magisterial interpretation; tested against actual hybrid cases (jurisdictions where doctrinal principles are constitutionalized alongside expert agencies).',
    'If hybrids are coherent, the relation downgrades to coexists_with and the kernel''s contest is political rather than logical; the engine''s computed foreclosure for this sibling pair would change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_authority_claims, conceptual, 'Whether the technocratic-magisterial authority contradiction is a true foreclosure.').

omega_variable(
    coordination_extraction_separability,
    'Is the regime''s genuine coordination function (shared benchmarks, predictable standards, commensurable safety claims) separable from its asymmetric cost-shifting, or does the coordination depend on the very metric frame that renders victim costs invisible?',
    'Counterfactual governance-design analysis: can distributionally weighted or rights-respecting metric frameworks deliver equivalent coordination benefits? Evidence from jurisdictions experimenting with distributional impact assessments and affected-community standing in standards processes.',
    'If separable, the cost-shifting is removable without losing coordination and reform is the structurally available remedy; if inseparable, the coordination function is itself coupled to the extraction and the constraint hardens toward the snare side as the frame matures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Separability of the regime''s coordination benefit from its extractive cost structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ai_g_tr_t0, observed).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(ai_g_tr_t4, observed).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement_basis(ai_g_tr_t8, observed).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(ai_g_tr_t12, observed).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(ai_g_tr_t16, observed).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(ai_g_tr_t20, observed).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(ai_g_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(ai_g_be_t0, observed).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement_basis(ai_g_be_t4, observed).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement_basis(ai_g_be_t8, observed).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement_basis(ai_g_be_t12, observed).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement_basis(ai_g_be_t16, observed).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(ai_g_be_t20, observed).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(ai_g_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(ai_g_su_t0, observed).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(ai_g_su_t4, observed).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement_basis(ai_g_su_t8, observed).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(ai_g_su_t12, observed).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(ai_g_su_t16, observed).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(ai_g_su_t20, observed).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(ai_g_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (ai_governance_legitimacy), four readings, four constraint stories with different epsilon, beneficiary/victim surfaces, and enforcement accounts. This file instantiates the technocratic_optimization_reading. The siblings author epsilon for the SAME standing arrangement through their own lights: the magisterial reading authors high epsilon (optimization violates doctrinal priority over the same referent), the democratic reading authors high epsilon on the consent dimension, the market_libertarian reading authors low epsilon but high suppression (the mandates themselves become the coercion). This story authors moderate epsilon (0.38) per the reading's own lights: real coordination benefit, real but non-dominant extraction. The manifest hypothesis (rope) was refined to tangled_rope on the declared victim surface plus active enforcement; uke_scope.hypothesis preserves the manifest's original value for cohort comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
