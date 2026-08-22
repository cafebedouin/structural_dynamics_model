% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: Existential-Risk Definition of AI Safety
 *   domain: technology governance/research community norms
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   ai_safety_commitment: the existential_risk_reading, on which AI safety
 *   means preventing extinction-level outcomes from misaligned
 *   superintelligent systems. The standing arrangement under contest (and
 *   therefore the epsilon referent, assessed by this reading's own lights) is
 *   the AI safety field's actual allocation of funding, legitimacy, and
 *   definitional authority as organized under this definition since roughly
 *   2014, intensifying sharply after 2023. The definition performs real
 *   coordination: it directs scarce talent and capital at a genuine
 *   free-rider problem. It also performs asymmetric transfer: it renders
 *   present-day algorithmic injuries categorically inadmissible as safety
 *   claims, marginalizes the researchers who study them, and hands frontier
 *   labs a safety narrative that covers continued capability scaling. The
 *   claim/metric gap is deliberate: claimed_type is tangled_rope from
 *   structural analysis (genuine coordination function plus identifiable
 *   payors plus active enforcement), while the metrics are authored
 *   independently as descriptive judgments; the engine computes per-seat
 *   types from the structural data. Sibling readings
 *   (near_term_harms_reading, dual_priority_reading) are separate constraints
 *   with their own epsilon values and victim sets, linked via
 *   network.affects_constraints; they are not folded into this file.
 *
 * KEY AGENTS:
 *   - - frontier_ai_labs: Primary beneficiary with enforcement role (institutional/arbitrage) — collects social license and definitional cover while bearing compliance costs and tail exposure
 *   - - xrisk_research_institutions: Secondary beneficiary (organized/identity_locked) — careers, mandates, and identities constituted by the framing
 *   - - ai_safety_funding_bodies: Agenda setter (institutional/mobile) — decides which safety work counts and gets funded
 *   - - present_harm_affected_populations: Primary target (powerless/trapped) — bear the exclusion of their injuries from the safety agenda
 *   - - near_term_safety_researchers: Target (moderate/constrained) — marginalized and reclassified within their own field
 *   - - open_source_developers: Prospective target (moderate/constrained) — bear compliance costs of threshold and pause proposals they did not draft
 *   - - ai_ethics_fairness_community: Excluded dissenter (organized/constrained) — contests the definitional monopoly from outside the defining bodies
 *   - - future_generations: Non-agent excluded constituency (powerless/trapped, civilizational/universal) — the reading's moral center, present only by proxy
 *   - - government_ai_regulators: Observer with emerging administrative role (institutional/analytical) — converts the community definition into enforceable structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.6).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "Existential-Risk Definition of AI Safety").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology governance/research community norms").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '57353998-67ba-43e5-b35f-761b4dc456c4').
narrative_ontology:cs_kernel_codification('57353998-67ba-43e5-b35f-761b4dc456c4', formalized).
narrative_ontology:cs_authority_grounding('57353998-67ba-43e5-b35f-761b4dc456c4', lineage).
narrative_ontology:cs_interpretation_layer_present('57353998-67ba-43e5-b35f-761b4dc456c4').
narrative_ontology:cs_reading_relation('57353998-67ba-43e5-b35f-761b4dc456c4', ai_safety_commitment__near_term_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('57353998-67ba-43e5-b35f-761b4dc456c4', ai_safety_commitment__dual_priority_reading, forecloses).
narrative_ontology:cs_axiom('57353998-67ba-43e5-b35f-761b4dc456c4', foundational, extinction_prevention_defines_safety).
narrative_ontology:cs_axiom_status(extinction_prevention_defines_safety, holdable).
narrative_ontology:cs_axiom_grounding('57353998-67ba-43e5-b35f-761b4dc456c4', extinction_prevention_defines_safety, empirically_contingent).
narrative_ontology:cs_axiom('57353998-67ba-43e5-b35f-761b4dc456c4', secondary, speculative_catastrophe_outweighs_documented_harm).
narrative_ontology:cs_axiom_status(speculative_catastrophe_outweighs_documented_harm, holdable).
narrative_ontology:cs_axiom_grounding('57353998-67ba-43e5-b35f-761b4dc456c4', speculative_catastrophe_outweighs_documented_harm, deontological).
narrative_ontology:cs_reference_frame('57353998-67ba-43e5-b35f-761b4dc456c4', extinction_prevention_canon).
narrative_ontology:cs_drift_state('57353998-67ba-43e5-b35f-761b4dc456c4', post_mainstreaming_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('57353998-67ba-43e5-b35f-761b4dc456c4', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, ai_safety_funding_bodies).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_harm_affected_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, near_term_safety_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, open_source_developers).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, longtermist_moral_weighting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the large-scale training runs the definition is about. Publish safety frameworks and responsible-scaling policies that translate the existential definition into internal practice. Collect reputational cover from demonstrating they take the ultimate risk seriously, and preemption of narrower regulation targeting present-day harms. Bear safety-team costs and face tail exposure should the reading harden into binding pause or licensing regimes. Exit means reframing messaging, relocating operations, or pivoting product lines.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, agenda_setter).

% Research organizations and field-building bodies whose missions, funding, and staff identities are constituted by the existential framing. Receive the largest share of dedicated safety philanthropy. Leaving the frame would dissolve the organization's reason to exist and its members' professional selves along with it.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, xrisk_research_institutions, beneficiary,
    organized, generational, identity_locked, global).

% Foundations and grantmakers that decide which work counts as safety and which gets funded under the definition. Currently allocate predominantly to existential-risk-aligned work, thereby setting the field's agenda. They could redirect portfolios comparatively easily, but doing so would forfeit the field-shaping position the definition affords them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_safety_funding_bodies, agenda_setter,
    institutional, generational, mobile, global).

% People subject to discriminatory, exploitative, or deceptive outputs of deployed systems today. Under the existential definition their injuries are classified as outside safety proper, so the safety budget, venues, and moral urgency pass them by. They cannot opt out of algorithmically mediated housing, employment, credit, or policing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_harm_affected_populations, payer,
    powerless, immediate, trapped, global).

% Researchers working on bias, misuse robustness, and documented harms. Their work is reclassified as ethics rather than safety, costing them access to safety-branded funding, venues, and standing. Exit means abandoning accumulated career capital and moving to adjacent fields at real cost.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_safety_researchers, payer,
    moderate, biographical, constrained, global).

% Small labs and open-source communities who would bear the compliance burden of compute thresholds, licensing schemes, and pause proposals justified by the existential definition. They lack the compliance staff of frontier labs and face disproportionate closure under such regimes, while having had little voice in drafting them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, open_source_developers, payer,
    moderate, immediate, constrained, global).

% The fairness and accountability research and advocacy community. They contest the definitional monopoly from adjacent conferences and journals but sit structurally outside the bodies that define safety. Within the frame, their objections register as category errors rather than rival claims on the same term.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_ethics_fairness_community, excluded,
    organized, biographical, constrained, global).

% The reading's central moral constituency: the people who would exist if catastrophe is averted. They cannot speak, contract, or vote, and are present only through proxy advocacy. Their absence is simultaneously the frame's moral engine and its accountability gap, since no one can check the proxy claims against their interests.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__existential_risk_reading, future_generations).

% National AI safety institutes and regulators that consume the definition when setting risk tiers, evaluation mandates, and reporting duties. They take testimony from the other seats and increasingly administer the definition into law, converting a community norm into enforceable structure.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, government_ai_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, government_ai_regulators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates fragmented research talent, funding, and policy attention onto the problem of ensuring advanced AI systems pursue human-compatible goals, solving a free-rider problem in which no single actor captures enough private return to fund pre-deployment alignment work at the needed scale.
% TRANSFER_FUNCTION: Moves funding, prestige, and definitional authority toward existential-risk-aligned research and institutions; moves attention and moral urgency away from present-day algorithmic harms and their victims; and imposes prospective compliance costs (compute thresholds, evaluations, pause obligations) on AI developers broadly.
% ABSENT_VOICES: Present-harm-affected populations and the fairness/accountability research community are structurally outside the definitional conversation; their objections are processed as category errors within the frame. Future generations, the reading's central moral constituency, are literally absent and represented only by interested proxies.
% DISAPPEARANCE_RATIONALE: If the definitional commitment vanished overnight, billions in aligned philanthropy, national AI safety institute mandates, lab responsible-scaling apparatus, and thousands of careers would reorganize around whichever rival definition captured the vacated semantic space; the near-term-harms and dual-priority readings would immediately contest it, and lab safety narratives built on the existential frame would lose their warrant.
% FOUNDING_PROBLEM: Early concern that sufficiently advanced AI systems could pursue goals incompatible with human survival, a speculative but potentially terminal failure mode that no market feedback loop or existing regulatory routine addresses before deployment.
% FOUNDING_PROBLEM_CORROBORATION: Partial external corroboration exists: repeated surveys of machine learning researchers find non-negligible median extinction-probability estimates, and government-commissioned assessments and national evaluation institutes treat catastrophic risk as serious enough to warrant state attention. However, the specific weighting that makes this THE definition of safety is attested almost exclusively within the beneficiary set (existential-risk institutions, aligned funders, and lab leadership), and no source outside the frame corroborates definitional supremacy; that limit is itself signal.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the coordination core is real (catastrophic-risk preparation solves a free-rider problem markets will not fund), but the same structure concentrates definitional authority and moral urgency on x-risk institutions and labs while the safety budget reaching present-harm victims approaches zero — the reading's own structural delta places high extraction on the speculative-technical-intervention complex and near-zero delivery on present algorithmic accountability. Suppression 0.60 is authored as a RAW structural property, unscaled by power or scope: it consists of definitional gatekeeping (peer review and hiring that treat near-term work as not-safety), funding gatekeeping, and discourse policing, with legal coercion arriving only at the margins via proposed compute gates. Theater 0.42: genuine interpretability and evaluation work coexists with summit communiques, pledge cycles, and safety-washing whose share peaked around the 2023 mainstreaming event and partially normalized afterward. Accessibility_collapse 0.45: the rival readings persist as live, institutionally represented alternatives, so the frame does not fully collapse them. Resistance 0.55: sustained fairness-community objection, accelerationist counter-mobilization, and the intra-field ethics-versus-safety schism. The temporal series run on ONE shared grid (2014-2026, eight points, all three metrics at every point). Base extractiveness rises monotonically with the framing's institutional capture; suppression_requirement dips in 2024 (a national deregulatory turn weakened one major jurisdiction's enforcement while others continued) before recovering; theater peaks at the 2023 pledge-and-summit cycle. End-state scalars match the 2026 grid values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the datum. From xrisk_research_institutions the definition is the field's constitution — the thing that makes their work legible and fundable. From present_harm_affected_populations the same definition is the mechanism that renders their injuries inadmissible: they are told their problem is real but is not THIS word. From frontier_ai_labs it is simultaneously cover (a safety narrative permitting scale-up) and insurance (a hedge against the tail their products create). From government_ai_regulators it is an administrable risk taxonomy. One structure, four experienced types; the engine derives this from power, exit, and directional position, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: frontier_ai_labs (collects license and cover; damped chi via arbitrage-grade exit), xrisk_research_institutions (collects funding and mandate; identity_locked exit deepens their fusion with the frame but they remain on the beneficiary end), ai_safety_funding_bodies (collect agenda power). Victims: present_harm_affected_populations (powerless and trapped, nearest the full-target end), near_term_safety_researchers (constrained exit, high d), open_source_developers (constrained, bearing prospective compliance costs). No directionality_overrides are authored, deliberately: the available override surface keys on power atoms, and this story places same-power agents on opposite sides (institutional beneficiaries versus institutional observers; organized beneficiaries versus an organized excluded dissenter), so any power-atom-level override would collide across seats. Where the canonical fallback likely understates a seat's target position (the excluded fairness community, whose derived d falls back toward symmetric despite real displacement), that residual is recorded here and in the cs_authority_framing omega rather than forced through a colliding override.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the constraint as pure rope — its own self-presentation, rational coordination against the ultimate risk — erases the payors: the populations whose claims the definition structurally refuses, and the researchers whose field position it taxes. Reading it as pure snare — the critics' safety-washing account — erases the genuine coordination function: alignment and interpretability research address a real free-rider problem that no market actor fully internalizes. Tangled_rope preserves both halves: the same structure that coordinates catastrophic-risk preparation transfers definitional authority away from rival harm-framings, and the transfer requires active enforcement (gatekeeping, funding discipline, discourse policing) to hold. On mandatrophy: the founding problem is contested rather than dead — the arrangement has not outlived its function so much as never settled whether its function is what it says. The live failure mode is forward-looking: if p(doom) estimates collapsed, the rope half would atrophy first and the structure would drift toward theatrical maintenance of a mandate whose referent dissolved — the measurement series exists to catch that transition early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is one reading of kernel ai_safety_commitment (reading: existential_risk_reading). What structurally changes if the sibling readings are instantiated instead?',
    'Author near_term_harms_reading and dual_priority_reading as separate stories over the same interval; compare victim sets, epsilon profiles, and computed seat classifications across the family.',
    'The near-term reading swaps the victim set to documented-harm populations and relocates extraction onto deployment decisions; the dual reading distributes extraction across both complexes and weakens the definitional gatekeeping channel. Classification of THIS reading is unaffected — the deltas belong to the siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Committer structure: sibling readings alter the victim set and the locus of extraction within the same kernel.').

omega_variable(
    doom_probability_wager,
    'What is the actual probability of extinction-level outcomes from misaligned advanced AI, and does it justify the demand weighting the definition imposes?',
    'Calibrated forecasting tournaments with resolved scoring, adversarial red-team evaluation of catastrophic-capability claims, and convergence testing across independently methoded estimate pipelines.',
    'A collapsing estimate would strip the coordination justification and expose the remaining structure as rent-seeking around a faded mandate; a robustly high estimate would strengthen the rope half and shift the tangled balance toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doom_probability_wager, empirical, 'Whether the empirical wager underwriting the definitional claim survives calibration.').

omega_variable(
    safety_washing_proportion,
    'What fraction of existential-risk-framed institutional activity is functional alignment and evaluation work versus social-license performance?',
    'Audit of safety-team outputs against deployment decisions, whistleblower and turnover testimony, and comparison of pledged versus enacted risk-threshold behavior across labs.',
    'A rising functional-to-performative gap would push the theater trajectory upward and signal drift toward snare-or-piton dynamics inside the tangled structure; a falling gap would support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_proportion, empirical, 'Functional versus performative composition of the x-risk-framed safety apparatus.').

omega_variable(
    displacement_causality,
    'Does the existential definition cause underinvestment in present-harm mitigation, or do the two agendas draw on separate resource pools?',
    'Funding-flow panel analysis around the 2023 framing shock: did near-term-harm funding fall, stagnate, or grow differentially relative to x-risk-aligned funding after the definition consolidated?',
    'Demonstrated causal displacement attributes the extraction asymmetry to this constraint specifically; pool-separation would relocate the asymmetry to the funding ecosystem and soften this story''s victim declarations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_causality, empirical, 'Whether the definitional monopoly causally displaces present-harm resources.').

omega_variable(
    intervention_false_assurance,
    'Do the speculative interventions the definition justifies (RLHF, interpretability, evaluations, pause and threshold regimes) actually reduce catastrophic risk, or do they manufacture assurance that enables faster scaling?',
    'Longitudinal tracking of intervention uptake against capability and incident indicators; natural experiments from jurisdictions adopting versus rejecting threshold regimes.',
    'If false assurance dominates, the coordination function inverts into an accelerant and classification pressure moves sharply toward snare; if interventions verifiably bind behavior, the rope half strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_false_assurance, empirical, 'Efficacy versus assurance-theater character of the definition''s favored interventions.').

omega_variable(
    cs_authority_framing_underdetermination,
    'Is lineage the right authority_grounding for this reading''s structure, or does extraction describe it better — and is the definitional commitment the right kernel framing at all, versus the funding-allocation regime that operates through it?',
    'Trace whether the reading''s authority traces to transmission from founding figures and texts (lineage) or to institutions whose funding depends on the kernel staying unrevised (extraction); test whether classifying the allocation regime separately yields a divergent pattern.',
    'An extraction-grounded framing would raise effective extraction and push classification toward snare; an allocation-regime framing would decompose this story further. The declared lineage-plus-interpretive-layer framing is the reading''s own self-understanding, which is exactly what a cover story would look like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_underdetermination, conceptual, 'CS framing under-determination: authority grounding and kernel level are contestable, and the alternatives classify differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2014, ai_safety_commitment__existential_risk_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement_basis(ai_s_tr_t2014, observed).
narrative_ontology:measurement(ai_s_tr_t2016, ai_safety_commitment__existential_risk_reading, theater_ratio, 2016, 0.24).
narrative_ontology:measurement_basis(ai_s_tr_t2016, observed).
narrative_ontology:measurement(ai_s_tr_t2018, ai_safety_commitment__existential_risk_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement_basis(ai_s_tr_t2018, observed).
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__existential_risk_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement_basis(ai_s_tr_t2020, observed).
narrative_ontology:measurement(ai_s_tr_t2022, ai_safety_commitment__existential_risk_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement_basis(ai_s_tr_t2022, observed).
narrative_ontology:measurement(ai_s_tr_t2023, ai_safety_commitment__existential_risk_reading, theater_ratio, 2023, 0.45).
narrative_ontology:measurement_basis(ai_s_tr_t2023, observed).
narrative_ontology:measurement(ai_s_tr_t2024, ai_safety_commitment__existential_risk_reading, theater_ratio, 2024, 0.44).
narrative_ontology:measurement_basis(ai_s_tr_t2024, observed).
narrative_ontology:measurement(ai_s_tr_t2026, ai_safety_commitment__existential_risk_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(ai_s_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2014, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2014, 0.3).
narrative_ontology:measurement_basis(ai_s_be_t2014, observed).
narrative_ontology:measurement(ai_s_be_t2016, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2016, 0.34).
narrative_ontology:measurement_basis(ai_s_be_t2016, observed).
narrative_ontology:measurement(ai_s_be_t2018, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement_basis(ai_s_be_t2018, observed).
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement_basis(ai_s_be_t2020, observed).
narrative_ontology:measurement(ai_s_be_t2022, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement_basis(ai_s_be_t2022, observed).
narrative_ontology:measurement(ai_s_be_t2023, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement_basis(ai_s_be_t2023, observed).
narrative_ontology:measurement(ai_s_be_t2024, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement_basis(ai_s_be_t2024, observed).
narrative_ontology:measurement(ai_s_be_t2026, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement_basis(ai_s_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2014, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2014, 0.25).
narrative_ontology:measurement_basis(ai_s_su_t2014, observed).
narrative_ontology:measurement(ai_s_su_t2016, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement_basis(ai_s_su_t2016, observed).
narrative_ontology:measurement(ai_s_su_t2018, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement_basis(ai_s_su_t2018, observed).
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement_basis(ai_s_su_t2020, observed).
narrative_ontology:measurement(ai_s_su_t2022, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement_basis(ai_s_su_t2022, observed).
narrative_ontology:measurement(ai_s_su_t2023, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement_basis(ai_s_su_t2023, observed).
narrative_ontology:measurement(ai_s_su_t2024, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2024, 0.54).
narrative_ontology:measurement_basis(ai_s_su_t2024, observed).
narrative_ontology:measurement(ai_s_su_t2026, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(ai_s_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI safety' conflates three structurally distinct commitments sharing one kernel. This story (existential_risk_reading) carries the extinction-prevention definition with its victim set of displaced present-harm constituencies and its epsilon concentrated on the speculative-intervention complex. The near_term_harms_reading sibling carries the documented-harm definition with a different victim set and epsilon centered on deployment decisions; the dual_priority_reading sibling refuses the exclusivity and distributes extraction across both. The upstream reading (this one) influences the downstream siblings because its definitional dominance sets the resource and legitimacy conditions under which they operate — its gatekeeping is their operating environment. Each member links the others via affects_constraints; no member hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
