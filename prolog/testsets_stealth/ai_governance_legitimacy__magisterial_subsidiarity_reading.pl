% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Magisterial Subsidiarity Reading of AI Governance Legitimacy
 *   domain: theological ethics / technology governance / political theology
 *
 * SUMMARY:
 *   A legitimacy claim over AI governance: arrangements are legitimate
 *   insofar as they conform to Catholic Social Doctrine — common good,
 *   subsidiarity, solidarity, universal destination of goods — as
 *   authoritatively interpreted by the Magisterium. The constraint
 *   coordinates the world's largest voluntary institutional network (Catholic
 *   health, education, development, and advocacy systems) around a dignity
 *   standard for AI, while reserving adjudication of legitimacy to a single
 *   interpretive authority that non-Catholic actors, dissenting theologians,
 *   and the declared beneficiaries themselves did not choose and cannot
 *   co-author. Genuine protection and interpretive-monopoly authority run
 *   through the same structure: conformity protects the vulnerable and
 *   simultaneously accrues interpretive authority to the adjudicating seat.
 *   This file instantiates one reading of the ai_governance_legitimacy kernel
 *   (see kernel_context); it authors a single, stable ε for that reading's
 *   own structure. KEY AGENTS (by structural relationship): - magisterium:
 *   agenda-setter and structural beneficiary (institutional/identity_locked)
 *   — authors and adjudicates the criterion; interpretive authority flows to
 *   it - private_tech_platforms: primary payer (institutional/constrained) —
 *   bears conformity demands; rejects the authority while co-opting its
 *   vocabulary - military_industrial_complex: payer
 *   (institutional/constrained) — targeted on autonomous weapons and
 *   surveillance; locked into AI by strategic competition -
 *   extractive_finance: payer (institutional/arbitrage) — most mobile target;
 *   weakest enforcement purchase against it - workers: declared beneficiary
 *   (organized/constrained) — protection delivered via unions and Catholic
 *   labor institutes; no adjudicative seat - global_south_communities:
 *   declared beneficiary (moderate/trapped) — absorb AI's extractive front;
 *   represented, not seated - families: declared beneficiary
 *   (moderate/trapped) — subsidiarity's protected unit; protection mediated
 *   through Catholic networks - marginalized_populations: declared
 *   beneficiary (powerless/trapped) — sharpest exposure; protection delivered
 *   through ecclesial witness - dissenting_catholic_ethicists: internal payer
 *   (moderate/identity_locked) — bear the interpretive monopoly's sharpest
 *   cost; admissible only under doctrinal review -
 *   secular_policy_institutions: excluded (institutional/constrained) — would
 *   object that no tradition holds interpretive monopoly; given no seat -
 *   catholic_institutional_network: beneficiary and enforcement vehicle
 *   (institutional/identity_locked) — implements the framework; its niche
 *   depends on it - ai_governance_researchers: analytical observer
 *   (analytical/analytical) — tracks whether principles alter deployment
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.46).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "Magisterial Subsidiarity Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological ethics / technology governance / political theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'fe314d8d-4604-4f76-aed9-81dbf216eda2').
narrative_ontology:cs_kernel_codification('fe314d8d-4604-4f76-aed9-81dbf216eda2', formalized).
narrative_ontology:cs_authority_grounding('fe314d8d-4604-4f76-aed9-81dbf216eda2', lineage).
narrative_ontology:cs_interpretation_layer_present('fe314d8d-4604-4f76-aed9-81dbf216eda2').
narrative_ontology:cs_reading_relation('fe314d8d-4604-4f76-aed9-81dbf216eda2', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_reading_relation('fe314d8d-4604-4f76-aed9-81dbf216eda2', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe314d8d-4604-4f76-aed9-81dbf216eda2', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('fe314d8d-4604-4f76-aed9-81dbf216eda2', foundational, magisterium_sole_authentic_interpreter).
narrative_ontology:cs_axiom_status(magisterium_sole_authentic_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('fe314d8d-4604-4f76-aed9-81dbf216eda2', magisterium_sole_authentic_interpreter, theological).
narrative_ontology:cs_axiom('fe314d8d-4604-4f76-aed9-81dbf216eda2', foundational, technology_subordinate_to_human_dignity).
narrative_ontology:cs_axiom_status(technology_subordinate_to_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('fe314d8d-4604-4f76-aed9-81dbf216eda2', technology_subordinate_to_human_dignity, deontological).
narrative_ontology:cs_axiom('fe314d8d-4604-4f76-aed9-81dbf216eda2', secondary, solidarity_participation_required_in_ai_governance).
narrative_ontology:cs_axiom_status(solidarity_participation_required_in_ai_governance, holdable).
narrative_ontology:cs_axiom_grounding('fe314d8d-4604-4f76-aed9-81dbf216eda2', solidarity_participation_required_in_ai_governance, deontological).
narrative_ontology:cs_reference_frame('fe314d8d-4604-4f76-aed9-81dbf216eda2', magisterial_common_good_order).
narrative_ontology:cs_drift_state('fe314d8d-4604-4f76-aed9-81dbf216eda2', contemporary_ai_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe314d8d-4604-4f76-aed9-81dbf216eda2', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_platforms).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, dissenting_catholic_ethicists).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterial_teaching_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the social corpus (encyclicals, the Rome Call for AI Ethics, Antiqua et Nova), adjudicates which AI governance arrangements conform, disciplines internal divergence through doctrinal offices, and represents the framework in UN fora. Interpretive authority and public standing flow to it from the framework's operation; the burden of maintaining doctrinal coherence across a global institutional network flows from it. Departing from the adjudicative role would dissolve the office's self-understanding as divinely entrusted teacher, so departure is not a live option.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, beneficiary).

% Develop and deploy the AI systems the framework governs. They bear conformity demands — transparency, accountability, oversight, limits on data extraction and labor displacement — and reject the framework's authority while selectively adopting its vocabulary (ethics boards, signature events). Operating openly outside the framework carries reputational cost in Catholic-majority markets and with Catholic institutional customers — health systems, universities — that represent significant demand.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_platforms, payer,
    institutional, biographical, constrained, global).

% Procures and deploys AI for defense; the framework names lethal autonomous systems and mass surveillance as dignity violations. Strategic competition locks defense establishments into AI adoption — they cannot leave AI, only contest the framework's authority. The Holy See's UN advocacy aims to bind them through international law they did not consent to.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, national).

% Moves capital under the framework's scrutiny — the universal destination of goods conditions the legitimacy of ownership patterns and speculative extraction. Capital is the most mobile factor: it relocates to permissive jurisdictions and instruments faster than the advocacy apparatus can track, making it the framework's weakest enforcement front.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    institutional, immediate, arbitrage, global).

% Face algorithmic displacement, surveillance-managed labor, and platform precarity. The framework's labor protections — worker dignity, just-wage norms applied to automation, participation requirements — reach them through unions, Catholic labor institutes, and ILO advocacy rather than through any seat they hold in the adjudication. They cannot exit labor markets; their protection depends on employers and states adopting the framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    organized, biographical, constrained, global).

% Absorb AI's extractive front — data annotation labor, e-waste, resource extraction for compute, deployment of untested systems. The framework's solidarity and universal-destination principles name their claims explicitly, and the Holy See advocates for them in UN fora. Their benefit is mediated: they are represented by the adjudicating authority and Catholic development agencies rather than seated as principals, and they cannot exit the global AI supply chain.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities, beneficiary,
    moderate, generational, trapped, global).

% Navigate AI-saturated education, care, and household economics. The tradition treats the family as the first natural society that subsidiarity shields against both market penetration and state substitution. Protections — limits on algorithmic child-targeting, recognition of care work — reach families through Catholic education and health networks; families themselves hold no governance seat and cannot exit the AI-saturated environment.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, generational, trapped, global).

% Bear the sharpest edge of unaccountable automated decisions — biased screening, exclusionary scoring, predictive policing. Protection of the vulnerable is the framework's most explicit operational demand, and ecclesial witness — Catholic hospitals, shelters, legal clinics — is where it is actually delivered. Coalition leverage exists mainly through Church structures; outside them these populations have little independent power.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, generational, trapped, global).

% Theologians and lay scholars who accept the substantive principles but contest particular interpretations — on automation policy, on the documents' treatment of surveillance, on the scope of participation. They bear the interpretive monopoly's sharpest internal cost: their scholarship is admissible only under doctrinal review, and divergence carries censure risk. Leaving the framework means leaving the vocation, since their professional identity is constituted within the tradition.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, dissenting_catholic_ethicists, payer,
    moderate, generational, identity_locked, global).

% Democratic regulators, UN technical bodies, and standards organizations govern AI through consent-based and expertise-based legitimacy. They would object that no religious tradition holds interpretive monopoly, but the framework's structure gives them no adjudicative seat: they may adopt conforming outcomes yet cannot co-author the criterion. They operate their own frameworks regardless, at the cost of being ruled deficient by this reading.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_policy_institutions, excluded,
    institutional, generational, constrained, global).

% Hospitals, universities, schools, development agencies, and investors that implement AI under the framework's guidance. It supplies their distinctive decision framework and moral identity in a field otherwise dominated by secular standards — a protected institutional niche. They are also the enforcement vehicle: ecclesial witness, procurement standards, and civil-society pressure are administered through this network. Adopting secular frameworks wholesale would dissolve the identity that distinguishes them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_institutional_network, agenda_setter).

% Scholars of technology governance who track whether the framework's principles alter actual deployment, whether its advocacy changes regulatory outcomes, and where its authority claims conflict with consent-based legitimacy. They collect nothing and bear no conformity demands; their seat is comparative analysis.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transnational moral-legal framework coordinating AI governance around a shared dignity standard: the common good as the criterion for deployment decisions, subsidiarity as the rule allocating decisions to the lowest competent level, solidarity as the mutual-obligation norm binding developers to those who bear AI's costs, and universal destination of goods as the distribution rule for technology's benefits. Without a shared criterion, Catholic institutions fragment across secular standards and the advocacy apparatus loses coherence; the framework solves that coordination problem for the world's largest voluntary institutional network.
% TRANSFER_FUNCTION: Moves conformity costs — transparency, accountability, oversight, limits on data extraction, labor displacement, and speculative finance — from AI-producing actors (platforms, defense establishments, financial capital) toward the protection of workers, families, Global South communities, and marginalized populations; and moves interpretive authority — the power to say what legitimate AI governance is — from all governance actors, including non-Catholic and dissenting ones, to the Magisterium.
% ABSENT_VOICES: Non-Catholic governance actors — democratic regulators, other religious traditions, secular ethicists, the firms themselves — hold no adjudicative seat: the criterion is authored and interpreted without them, and their consent is neither sought nor required. Within the tradition, dissenting theologians are heard only under doctrinal review. The Global South populations in whose name the framework advocates are represented by the Magisterium and Catholic agencies rather than seated as principals — a representation structure the beneficiaries themselves did not choose.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the commercial AI landscape would barely register — its targets largely ignore it already. But the Catholic institutional world would rearrange: hospitals, universities, schools, and development agencies would lose their distinctive AI-governance criterion and drift toward secular standards ad hoc; the Holy See's UN advocacy on AI would stop; the Rome Call signatory structure would dissolve; dissenting Catholic ethicists would lose the doctrinal frame they contest. A global institutional network's governance arrangements demonstrably depend on it, even where the commercial core does not.
% FOUNDING_PROBLEM: The escape of technological and economic power from moral governance: systems that organize social life while reducing persons to instruments and externalizing harms onto those with least power. The social corpus was built from Rerum Novarum through Laudato Si' to subordinate markets and technology to the common good; the AI-specific articulation extends that founding problem to a governance vacuum in which development outpaces oversight, efficiency logics crowd out dignity, and the vulnerable absorb unaccountable automated decisions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: secular governance initiatives independently identify the same coordination vacuum — UNESCO's AI ethics recommendation, the EU AI Act's risk framework, national AI safety institutes — attesting that the oversight gap and the harm-to-the-vulnerable problem are real even while rejecting the Magisterial solution. Industry safety research and civil-society algorithmic-harm audits corroborate the empirical core. No secular source attests that Magisterial adjudication is the remedy: corroboration extends to the problem, not to this reading's authority claim.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.52 because two strands run through one structure: substantive conformity demands on AI-producing actors (transparency, oversight, limits on extraction and displacement), and interpretive deference demanded from every actor — including non-Catholics and internal dissenters who never consented to the adjudicating authority. Both strands are real; both are entangled with genuine delivery (protection of the vulnerable, a workable decision framework for a global institutional network). Suppression is authored at 0.46 as a raw structural property — it is not scaled by power or scope; only extractiveness is scaled in the engine's computation. Enforcement is non-coercive at the individual level but institutionally real: doctrinal censure of internal divergence, foreclosure of rival readings within the framework, public delegitimation of secular alternatives. Its mechanism is mostly structural (censure machinery, adjudicative foreclosure) with an internalized component among internal dissenters (self-restraint under doctrinal review); the split is carried by the enforcement_vs_identity_persistence and kernel_revision_vs_interpretation omegas. Theater at 0.38: a substantial share of activity is performative witness — documents, signature events, conferences with limited operative reach into the commercial landscape — but real functional activity exists in Catholic institutional procurement, ethics boards, and UN advocacy with concrete text. Accessibility_collapse at 0.30: alternatives are not collapsed — secular frameworks dominate actual practice — the constraint collapses alternatives only inside the Catholic institutional world. Resistance at 0.55: the constraint is more ignored or co-opted than fought, but explicit rejection from market-libertarian and technocratic actors, plus uneven implementation among Catholic institutions, is real. Claim and metrics are independent authored facts: I claim the entangled coordination-plus-extraction type because the structure genuinely solves a collective-action problem (AI's race dynamics and externalized harms, for a real constituency) while asymmetrically accruing interpretive authority to one seat; the engine computes per-seat classifications from the structural data, and divergence between my claim and any computed seat type is signal, not error.
 *   
 *   The measurement series run on one shared time grid (T0≈2000 to T25≈2025) so every tracked metric is authored at every examined point. Base extractiveness rises with AI's growing share of the economy and governance — the same claim covers a vastly larger domain in 2025 than in 2000. Suppression_requirement is authored because the story specifically tracks enforcement-capacity change: the apparatus (dicastery structures, the Rome Call process, expanded UN advocacy, the 2025 AI document) was built up and hardened over the interval. Theater falls as the apparatus professionalized — early engagement was almost purely aspirational documents; implementation machinery now exists — while extraction and suppression rise: the constraint is intensifying and professionalizing simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's seat the constraint is the faithful application of a divinely-entrusted teaching office to a new domain — coordination, not extraction; the deference it collects is owed, not taken. From the payer seats it is an unconsented authority claim with costs and no seat at the table — and these seats diverge among themselves despite sitting at the same institutional power level: finance's arbitrage-grade mobility lets it relocate faster than advocacy tracks, while defense's strategic lock-in and platforms' market entanglement hold them in place; identical global standing, different effective positions, driven entirely by constraint-specific exit structure. From the internal payer seat (dissenting ethicists) it is a genuine tradition whose interpretive monopoly narrows the tradition it serves. From the declared beneficiaries it is mostly aspirational: real protection delivered through mediation, with no seat of their own. Identity lock binds three seats — the Magisterium (institutional identity fusion: the office IS its teaching function), the Catholic institutional network (the organization has become its mission), and dissenting ethicists (professional identity constituted within the tradition); if any of these identity frames broke, their computed positions would change sharply — the ethicists most of all, since they alone could convert identity-lock into exit. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations map to real structural positions. Workers, Global South communities, families, and marginalized populations are the intended protection targets — low directionality, damped effective extraction; their trapped exits stabilize that position (they cannot leave AI-saturated environments, so the protection, where real, is the only shelter available). The Magisterium and the Catholic institutional network are structural beneficiaries with identity-locked exits — the constraint subsidizes their authority and identity, and they cannot exit without dissolving what they are. Platforms, defense, and finance are targets — high directionality — differentiated by exit: finance's arbitrage damps its effective extraction; defense and platforms sit nearer full-target. Dissenting ethicists are targets with identity-locked exits — plausibly the sharpest per-capita burden in the structure, since they cannot exit without leaving the vocation. Secular policy institutions sit outside the beneficiary/victim data; they are contested by the constraint rather than subsidized or burdened by it. No directionality overrides are used: the declarations plus exit differentiation already produce the seat structure, and the override mechanism is keyed to power atoms — applying one would flatten the institutional-atom differences this story depends on, since the institutional atom here contains beneficiaries, targets, and an excluded seat simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — technological power escaping moral governance — is live, arguably more acute than at the framework's founding, so this is not a mandatrophy case and mandatrophy_resolved is not declared. The entangled classification does protective work in both directions: it prevents mislabeling the constraint as pure coordination, which would launder the interpretive monopoly as mere teaching; and it prevents mislabeling it as pure extraction, which would erase the genuine protection delivered to populations with no other institutional advocate at comparable scale. The receipt surface sharpens this: gains demonstrably accrue to the Magisterium's interpretive authority — a named, captured seat — while fixing the constraint's excesses is prohibitive for the only actor who could fix it, because revising the adjudicative claim would fracture doctrinal continuity across centuries of social teaching and cost more than any benefit the Magisterium could expect. That combination — captured receipt atop genuine coordination delivery, with a prohibitive cost-to-fix — is precisely the entangled structure the category exists to name, and the R5 interview corroborates it: the founding problem is attested live by sources outside the benefiting parties, so the arrangement is neither zombie nor cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the magisterial_subsidiarity_reading of the kernel ai_governance_legitimacy; the sibling readings (technocratic_optimization_reading, democratic_pluralist_reading, market_libertarian_reading) instantiate different constraints from the same kernel. How much of the structure measured here is specific to Magisterial adjudication versus shared with any dignity-based framework — that is, if a sibling held the same substantive principles with a different adjudicator, what would change structurally?',
    'Cross-reading comparison across the four family stories: identify which structural elements (beneficiary set, victim set, enforcement machinery, extraction profile) are invariant across readings and which are unique to the Magisterial adjudication claim.',
    'If the substantive principles are separable from Magisterial adjudication, most of this reading''s measured extraction is authority-rent rather than ethical-content cost; the reading''s distinctive contribution would be its authority claim, not its principles, and sibling stories would show near-identical coordination with different extraction seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'What is reading-specific versus kernel-invariant in the measured structure.').

omega_variable(
    interpretive_rent_vs_conformity_cost,
    'Of the constraint''s measured extraction, how much accrues from the Magisterium''s interpretive monopoly — deference demanded from actors who never consented to it, including internal dissenters — versus the substantive cost of dignity-conformity itself (transparency, oversight, limits on extraction and displacement)?',
    'Counterfactual pricing: cost the substantive requirements against secular equivalents (EU AI Act compliance, UNESCO-recommendation implementation, algorithmic-impact-assessment regimes) and attribute the residual extraction to the authority claim itself.',
    'If most extraction is interpretive rent, the constraint leans toward pure extraction riding on a genuine teaching function; if most is conformity cost, the entangled coordination-plus-extraction reading is confirmed and the monopoly is a secondary rider on substantive demands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_rent_vs_conformity_cost, empirical, 'Decomposing extraction into authority-rent and substantive-conformity components.').

omega_variable(
    mediated_beneficiary_structure,
    'Do the declared beneficiaries — workers, Global South communities, families, marginalized populations — actually receive the framework''s protection, or is the benefit largely aspirational, claimed on their behalf by an adjudicating authority in which they hold no seat?',
    'Outcome comparison of AI deployments inside Catholic institutional networks (health algorithms, education platforms, supply-chain tools) against comparable secular deployments, tracking outcomes for the declared beneficiary populations.',
    'If outcomes are indistinguishable, the beneficiary declarations are aspirational and the coordination function thins toward pure authority-claim; if measurable, the coordination-plus-extraction structure is confirmed with genuine delivery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mediated_beneficiary_structure, empirical, 'Whether declared protection reaches the populations in whose name it is claimed.').

omega_variable(
    enforcement_vs_identity_persistence,
    'Does the constraint persist because its enforcement machinery — moral suasion, advocacy, ecclesial witness — holds it, or because the Catholic institutional network''s identity requires it regardless of enforcement efficacy?',
    'Compare framework-conformity in Catholic institutions facing different enforcement intensity (diocesan versus religious-order versus lay-run institutions; jurisdictions with and without Holy See diplomatic pressure) — uniform compliance under varied enforcement indicates identity, not enforcement, is load-bearing.',
    'If identity holds it, the enforcement picture overstates active maintenance and the Catholic core is more inertial than enforced — degraded-inertial dynamics within the core while the advocacy surface remains partly performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_identity_persistence, empirical, 'Whether enforcement or institutional identity sustains the constraint.').

omega_variable(
    kernel_revision_vs_interpretation,
    'Do the AI-specific documents (Rome Call for AI Ethics, Antiqua et Nova) interpret a fixed kernel or revise it de facto — and does the claim of mere application mask drift absorption in the interpretive layer?',
    'Doctrinal-genealogy analysis: test whether the AI-specific norms are derivable from the pre-AI social corpus by accepted interpretive methods, or introduce normative content with no antecedent.',
    'If revision is masked as interpretation, the interpretive layer is absorbing drift without surfacing it — a drift-denying commitment-system pattern that raises effective extraction by denying the governed any record of what changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_revision_vs_interpretation, conceptual, 'Whether AI-era teaching is interpretation or unacknowledged kernel revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magisterial_subsidiarity_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement_basis(magisterial_subsidiarity_tr_t0, observed).
narrative_ontology:measurement(magisterial_subsidiarity_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement_basis(magisterial_subsidiarity_tr_t5, observed).
narrative_ontology:measurement(magisterial_subsidiarity_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(magisterial_subsidiarity_tr_t10, observed).
narrative_ontology:measurement(magisterial_subsidiarity_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(magisterial_subsidiarity_tr_t15, observed).
narrative_ontology:measurement(magisterial_subsidiarity_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(magisterial_subsidiarity_tr_t20, observed).
narrative_ontology:measurement(magisterial_subsidiarity_tr_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(magisterial_subsidiarity_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(magisterial_subsidiarity_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(magisterial_subsidiarity_be_t0, observed).
narrative_ontology:measurement(magisterial_subsidiarity_be_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(magisterial_subsidiarity_be_t5, observed).
narrative_ontology:measurement(magisterial_subsidiarity_be_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(magisterial_subsidiarity_be_t10, observed).
narrative_ontology:measurement(magisterial_subsidiarity_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(magisterial_subsidiarity_be_t15, observed).
narrative_ontology:measurement(magisterial_subsidiarity_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(magisterial_subsidiarity_be_t20, observed).
narrative_ontology:measurement(magisterial_subsidiarity_be_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(magisterial_subsidiarity_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(magisterial_subsidiarity_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(magisterial_subsidiarity_su_t0, observed).
narrative_ontology:measurement(magisterial_subsidiarity_su_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(magisterial_subsidiarity_su_t5, observed).
narrative_ontology:measurement(magisterial_subsidiarity_su_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(magisterial_subsidiarity_su_t10, observed).
narrative_ontology:measurement(magisterial_subsidiarity_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement_basis(magisterial_subsidiarity_su_t15, observed).
narrative_ontology:measurement(magisterial_subsidiarity_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(magisterial_subsidiarity_su_t20, observed).
narrative_ontology:measurement(magisterial_subsidiarity_su_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement_basis(magisterial_subsidiarity_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI governance legitimacy' decomposes into four structurally distinct constraints — one per reading of the kernel. Each reading has its own ε, beneficiary/victim structure, and enforcement profile: this story's ε (0.52) is authored for the Magisterial-conformity arrangement as this reading assesses it; a sibling reading of the same landscape authors a different ε over a different structure. The stories form a constraint family linked by these edges. Downstream pressure runs from this reading toward the others: its UN advocacy and signature structures change the legitimacy conditions under which technocratic and market governance operate (dignity objections now have an organized institutional voice), without resolving the dispute. The identity_coordination typing is checked against the gaming risk: the boundary-maintenance function is genuine (a 1.3-billion-member institutional network would fragment without a shared criterion), and the extraction that rides on it — the interpretive monopoly — is separately visible in the victim declarations and the receipt surface rather than hidden inside the identity framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
