% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: AI Safety as Existential-Risk Prevention (Definitional Constraint)
 *   domain: technology_governance/research_funding/epistemic_authority
 *
 * SUMMARY:
 *   This file instantiates the existential_risk_reading of the
 *   ai_safety_commitment kernel. The arrangement under examination is the
 *   operative definition — dominant in funding gatekeeping, hiring, venue
 *   norms, and governance frameworks since roughly 2014 — that 'AI safety'
 *   means preventing extinction-level outcomes from misaligned
 *   superintelligent systems. The ε referent is this standing arrangement
 *   itself, assessed from this reading's own seat: the reading holds the
 *   threat model is real and the coordination is necessary, and it still
 *   authors substantial extraction, because the arrangement channels large
 *   resource flows into speculative interventions whose risk-reduction is
 *   unverifiable, converts safety concern into scaling license for frontier
 *   labs, classifies present-day algorithmic harms out of the safety
 *   category, and makes claims on behalf of future people who cannot
 *   represent themselves. The sibling readings (near_term_harms_reading,
 *   dual_priority_reading) are separate constraint files; per the
 *   ε-invariance principle this story carries one stable ε for one reading
 *   and does not hedge across them. KEY AGENTS (by structural relationship):
 *   frontier_ai_labs: agenda-setter and net capturer
 *   (institutional/arbitrage) — writes and enforces the definition, collects
 *   the legitimacy rent; xrisk_funding_foundations: agenda-setter
 *   (powerful/arbitrage) — gates the field's funding by the predicate;
 *   alignment_research_institutes: beneficiary (organized/identity_locked) —
 *   careers fused with the premise; ai_governance_institutes: beneficiary
 *   (institutional/constrained) — mandates written around the framing;
 *   future_humanity: primary target (powerless/trapped, civilizational
 *   horizon) — bears the full downside of the unverifiable bet;
 *   present_algorithmic_harm_sufferers: secondary target
 *   (powerless/constrained) — harms classified out of scope;
 *   near_term_harm_researchers: excluded contesters (moderate/mobile);
 *   independent_risk_analysts: analytical observer — the outside check on the
 *   arrangement's self-description.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety as Existential-Risk Prevention (Definitional Constraint)").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology_governance/research_funding/epistemic_authority").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '1cdbab08-83c5-4c0a-919b-78b433163659').
narrative_ontology:cs_kernel_codification('1cdbab08-83c5-4c0a-919b-78b433163659', distributed).
narrative_ontology:cs_authority_grounding('1cdbab08-83c5-4c0a-919b-78b433163659', extraction).
narrative_ontology:cs_interpretation_layer_present('1cdbab08-83c5-4c0a-919b-78b433163659').
narrative_ontology:cs_reading_relation('1cdbab08-83c5-4c0a-919b-78b433163659', ai_safety_commitment__near_term_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('1cdbab08-83c5-4c0a-919b-78b433163659', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('1cdbab08-83c5-4c0a-919b-78b433163659', foundational, extinction_risk_definitional_primacy).
narrative_ontology:cs_axiom_status(extinction_risk_definitional_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1cdbab08-83c5-4c0a-919b-78b433163659', extinction_risk_definitional_primacy, empirically_contingent).
narrative_ontology:cs_axiom('1cdbab08-83c5-4c0a-919b-78b433163659', foundational, catastrophic_asymmetry_justifies_speculative_preemption).
narrative_ontology:cs_axiom_status(catastrophic_asymmetry_justifies_speculative_preemption, holdable).
narrative_ontology:cs_axiom_grounding('1cdbab08-83c5-4c0a-919b-78b433163659', catastrophic_asymmetry_justifies_speculative_preemption, instrumental).
narrative_ontology:cs_reference_frame('1cdbab08-83c5-4c0a-919b-78b433163659', xrisk_definitional_primacy).
narrative_ontology:cs_drift_state('1cdbab08-83c5-4c0a-919b-78b433163659', contemporary_post_summit_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1cdbab08-83c5-4c0a-919b-78b433163659', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, xrisk_funding_foundations).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, alignment_research_institutes).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, ai_governance_institutes).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_algorithmic_harm_sufferers).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, intelligence_explosion_premise).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, longtermist_moral_weighting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy frontier AI systems; publish safety frameworks and responsible-scaling policies framed around catastrophic risk; fund safety teams sized far below capability spending. The definition gives them the social license to continue scaling — addressing the gravest risk — and they shape what counts as safety through hiring, publication venues, and governance engagement. Exit is cheap for them: framings can be rebranded, commitments re-scoped, regulators engaged elsewhere.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, beneficiary).

% Gate the field's primary private funding stream: grants, field-building, prizes, and career support flow by the definitional predicate. The definition is their portfolio theory, and they convert it into agenda-setting power over what the field studies. Redirecting capital is administratively easy; giving up the agenda-setting position is the real cost of revising the definition.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, xrisk_funding_foundations, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, xrisk_funding_foundations, beneficiary).

% Produce interpretability, alignment, and governance research under the extinction premise. Their funding, hiring pipelines, and scholarly reputations are built on the premise holding, and individual researchers' careers are fused with it — entertaining the sibling definitions devalues their own positions. Leaving means dissolving professional identity built over a decade.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, alignment_research_institutes, beneficiary,
    organized, generational, identity_locked, global).

% State-backed AI safety institutes adopt the catastrophic-risk framing into their mandates, evaluation regimes, and budgets. The definition gives them standing and headcount; present-harm remits sit outside their charters, and revising mandates requires legislative action they do not control.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_governance_institutes, beneficiary,
    institutional, biographical, constrained, national).

% The people whose survival the definition claims to secure. They bear the entire downside if the interventions funded on their behalf fail to deliver, and the spending is unverifiable on any clock they could hold anyone to. They cannot represent themselves, contest allocations, or decline the bet; every claim made in their name is authored by present agents who also collect from the arrangement.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% People harmed now by deployed systems — discriminatory screening in hiring, housing, and credit; exploitative data-labeling labor; misinformation exposure. The definitional predicate classifies their harms as outside 'safety,' so accountability work competes for resources outside the definition's funding channels and attention. Exiting algorithmically mediated housing, employment, and credit is not available to them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_algorithmic_harm_sufferers, payer,
    powerless, immediate, constrained, global).

% Fairness, accountability, and transparency researchers whose work is reclassified as 'not safety' under the definition. They contest the framing in publications and policy comments but sit outside the definition's funding channels, conference tracks, and governance seats. Rebranding into 'responsible AI' is possible but costs standing inside safety venues.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harm_researchers, excluded,
    moderate, immediate, mobile, global).

% Academic critics, forecasters, and science-and-technology-studies scholars with no stake in the definition's survival. They track how the definition channels money and legitimacy, and they assess both the risk premise and the field's delivered output; their audits are the main outside check on the arrangement's self-description.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, independent_risk_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research talent, funding, and governance attention on a single catastrophic threat model — misaligned superintelligence — addressing a genuine collective-action problem: preventing a global catastrophe no single actor can address alone, requiring preemptive work before the threat materializes. Also gives labs, funders, and governments a shared criterion for what counts as safety work.
% TRANSFER_FUNCTION: Moves money, talent, and regulatory attention from dispersed present-day concerns toward speculative alignment research and catastrophic-risk governance; moves legitimacy to frontier labs that can claim to be addressing the gravest risk; moves the unrepresentable interests of future people into the agenda control of present agents who claim to speak for them.
% ABSENT_VOICES: Future people are structurally absent and cannot object; every claim on their behalf is authored by agents who collect from the arrangement. Present-day harm sufferers are present but pre-emptively classified out of the category ('that is not safety'). Near-term-harm researchers are excluded from the definition's funding channels and governance seats — they object from outside the room the definition built.
% DISAPPEARANCE_RATIONALE: Funding streams would re-route toward whichever definition replaced this one, safety teams would re-scope or dissolve, governance frameworks would be rewritten around the new predicate, and the near-term harms agenda would gain the resources and standing the definition currently commands; the field's career structure would reorganize within years.
% FOUNDING_PROBLEM: Early AI safety (pre-2015) coalesced around a specific recognition: sufficiently capable systems might pursue objectives misaligned with human values, with no correction possible after deployment — a problem that, if real, dominates all present harms and demands preemptive technical work before capabilities arrive.
% FOUNDING_PROBLEM_CORROBORATION: The risk scenario's standing is corroborated outside the beneficiary set: the 2023 extinction-risk statement drew signatories with no stake in the definition's funding streams, and state risk assessments treat catastrophic AI risk as a real scenario. But the status claim — that this problem should definitionally dominate 'AI safety' over present harms — is attested almost entirely by the parties the definition funds and legitimates, and is explicitly disputed by holders of the sibling readings. Partial outside corroboration of the problem; near-zero outside corroboration of the definitional priority.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.68: post-2023 funding surges flow disproportionately to speculative interventions — interpretability agendas, alignment research, pause/slowdown governance — whose risk-reduction output cannot be verified on any near-term clock, while the definition simultaneously strips resources from cheap, verifiable accountability work. Suppression is 0.58 and structural rather than physical — and it is authored as a raw structural property, unscaled by power or scope; only extractiveness gets scaled downstream: the definition is enforced through grant gatekeeping, hiring and venue norms, and the epistemic authority of P(doom)-style forecasting, so alternative framings are not banned but defunded and reclassified. Theater is 0.45: a real technical core (interpretability, evaluations) operates inside a growing shell of safety frameworks, commitments, and summitry that bind no deployment decision. Accessibility_collapse is 0.50 — alternatives persist and are actively contested rather than collapsed; resistance is 0.60 — the near-term and dual-priority camps contest the definition in publications, policy processes, and funding disputes. All three measurement series share one time grid (0,2,4,6,8,10,12) so no metric is sampled against another's end-state. The suppression series is authored because this story specifically tracks enforcement-capacity growth: the definitional enforcement machinery (gatekeeping, codified lab safety frameworks, institutionalized safety institutes) matured and hardened over the interval. Claim and metrics are authored independently: the reading claims tangled_rope because the coordination function is genuine on its own premise — a real catastrophic risk requires collective preemption — while the metrics describe an operation whose extraction has grown with its budget. On coalition: future_humanity is the one payer seat for which coalition power is structurally impossible (non-existence is the ultimate coordination barrier); present_algorithmic_harm_sufferers could in principle coalition but are definitionally fragmented across harm types, which is itself an enforcement effect of the predicate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute opposite types from the same structure. From the labs' seat the definition is the field's legitimate constitution, one they helped write and from which they draw license; from future_humanity's seat — powerless, trapped, civilizational horizon — the same arrangement is an unaccountable bet placed on their behalf by agents who collect from it, carrying the maximum-directionality exposure of any seat in the story despite being its claimed beneficiary. Present-day harm sufferers experience the arrangement as categorical exclusion: their harms are not undercompensated, they are definitionally out of scope. The engine derives this divergence from the structural data; the gap between claimed protection and structural exposure for the future-humanity seat is the perspectival divergence this story exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the beneficiary end: frontier_ai_labs (agenda_setter/secondary beneficiary, arbitrage exit — the derivation reads their net position correctly since legitimacy gains exceed safety-team costs), xrisk_funding_foundations (arbitrage exit, near-full beneficiary), alignment_research_institutes (identity_locked exit locks them further into the subsidy side), ai_governance_institutes (constrained, mandate-dependent). Victim declarations drive d toward the target end: future_humanity (powerless + trapped → nearest the full-target end of any seat; trapped exit and total powerlessness place them at maximum effective exposure despite being the arrangement's claimed protectees) and present_algorithmic_harm_sufferers (powerless + constrained → high exposure). No directionality overrides are declared: the derivation from declared roles, power atoms, and exit options reproduces the structural relationships without correction. The one genuinely dual-positioned agent, frontier_ai_labs, is authored agenda_setter with secondary beneficiary — they pay safety-team costs but net-capture the legitimacy rent, which is why gain_flow names them.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's public rhetoric carries a false-summit flavor: superintelligence arrival is presented as an inevitable trajectory — natural law rather than a contested forecast — which would naturalize a constructed, beneficiary-bearing arrangement. Declaring the beneficiaries and authoring honest metrics keeps the constraint in contested-arrangement space (tangled_rope) instead of letting the inevitability framing certify it as a mountain; the FSM machinery would fire if this were claimed as mountain, and the omega on the empirical basis of the premise documents exactly the natural-law-versus-constructed ambiguity. On the mandatrophy axis: the founding problem was framed for a small pre-2015 research community; the arrangement has since become a funding-and-governance apparatus employing thousands. Its founding problem's status is contested rather than dead, so the capture flag does not fire — but if timelines recede without definitional revision, the apparatus persists by inertia and the trajectory runs toward piton: administrators who could pluralize the definition but will not forfeit their position, diffuse costs, theatrical maintenance. The contested × world_rearranges cell is the one to watch in cross-reading comparison: the sibling readings author different founding-problem statuses over the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (existential_risk_reading) of the kernel ai_safety_commitment; what would instantiating a sibling reading change structurally, and where exactly is the disagreement located?',
    'Which definitional predicate a funding charter, governance framework, or field institution adopts. near_term_harms_reading swaps the victim set to present-day harmed populations and relocates the extraction profile to unchecked deployment; dual_priority_reading dissolves the crowding-out structure by authorizing both portfolios non-competingly.',
    'Sibling instantiations produce different victim sets, different ε profiles (high ε on speculative interventions here versus high ε on unchecked deployment there), and different classifications; this file''s ε is invariant only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the AI-safety kernel; disagreement located in the definitional predicate and the time horizon governing allocation.').

omega_variable(
    p_doom_empirical_basis,
    'How probable and how imminent is catastrophe from misaligned superintelligence? The proportionality of the arrangement''s spending depends on it: at high probability and short timelines, speculative spending is proportionate coordination cost; at low probability or long timelines, the same spending is rent collected on an unverifiable premise.',
    'Calibrated forecasting track records of timeline claimants, independent red-teaming of the threat models, and compute-growth empirics assessed outside the funded institutions.',
    'High assessed probability lowers effective ε (coordination cost dominates); low assessed probability raises it (rent on an unverifiable premise) and pushes the arrangement toward snare or piton readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p_doom_empirical_basis, empirical, 'Whether the empirical premise of the definitional claim is strong enough to classify the spending as coordination rather than extraction.').

omega_variable(
    alignment_progress_verifiability,
    'Do interpretability and alignment interventions produce verifiable risk reduction, or unfalsifiable reassurance? The theater_ratio and the coordination-versus-extraction split turn on this.',
    'Adversarial evaluation: pre-registered benchmarks the interventions could fail, and red-team attempts to elicit misbehavior the methods claim to catch.',
    'If outputs are verifiable, measured theater overstates dysfunction and ε falls toward the coordination-cost floor; if unfalsifiable, a large share of the arrangement''s activity is performance and ε rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_progress_verifiability, empirical, 'Verifiability of the technical output funded under the definition.').

omega_variable(
    safety_washing_decoupling,
    'Are frontier labs'' catastrophic-risk safety commitments decoupled from deployment decisions — does the definition function partly as scaling license?',
    'Audit whether safety evaluation results actually gate training and deployment decisions, comparing announced commitments against shipped behavior across release cycles.',
    'Decoupling confirms a cover-story component in the beneficiary structure (labs as net capturers of legitimacy) and raises effective extraction; coupling would support the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_decoupling, empirical, 'Whether the labs'' beneficiary position includes a legitimacy-cover component.').

omega_variable(
    crowding_out_magnitude,
    'Are existential-risk and present-harm budgets additive or competing? The reality of the present_algorithmic_harm_sufferers victim claim depends on whether definition-driven allocation actually displaces accountability work.',
    'Funding-flow analysis across the interval: track whether growth in x-risk-allocated funding correlates with decline in fairness and accountability funding, net of total-field growth.',
    'If budgets are additive, the crowding-out victim claim weakens and the victim set shrinks toward future_humanity alone; if competing, the present-sufferer seat is a full payer and the suppression of the alternative framing is materially costly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_magnitude, empirical, 'Magnitude of resource displacement from present-harm work under the definition.').

omega_variable(
    definitional_suppression_mechanism,
    'Is the suppression of alternative framings structural (grant gatekeeping, venue norms, hiring pipelines) or internalized (researchers self-censor into the framing because the definition determines fundability and standing)?',
    'Post-exit trajectory: whether researchers who leave x-risk-defined institutions resume plural-framing work, or carry the framing''s categories with them.',
    'If internalized, the arrangement''s suppression outlives its enforcement machinery — pluralizing the definition on paper would not restore the suppressed agenda, and effective suppression exceeds the structural measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definitional_suppression_mechanism, conceptual, 'Structural versus internalized mechanism behind the measured suppression of alternative safety framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t2, ai_safety_commitment__existential_risk_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(ai_s_tr_t2, observed).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__existential_risk_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(ai_s_tr_t4, observed).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__existential_risk_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(ai_s_tr_t6, observed).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__existential_risk_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(ai_s_tr_t8, observed).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__existential_risk_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(ai_s_tr_t10, observed).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__existential_risk_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement_basis(ai_s_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__existential_risk_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t2, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement_basis(ai_s_be_t2, observed).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__existential_risk_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(ai_s_be_t4, observed).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__existential_risk_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement_basis(ai_s_be_t6, observed).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__existential_risk_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(ai_s_be_t8, observed).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__existential_risk_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(ai_s_be_t10, observed).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__existential_risk_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(ai_s_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__existential_risk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t2, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement_basis(ai_s_su_t2, observed).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__existential_risk_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(ai_s_su_t4, observed).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__existential_risk_reading, suppression_requirement, 6, 0.49).
narrative_ontology:measurement_basis(ai_s_su_t6, observed).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__existential_risk_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement_basis(ai_s_su_t8, observed).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__existential_risk_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(ai_s_su_t10, observed).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__existential_risk_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(ai_s_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI safety' decomposes, per the ε-invariance principle, into three structurally distinct constraints sharing the kernel ai_safety_commitment. This file is the existential_risk_reading: victim set = future humanity plus crowded-out present sufferers; ε concentrated on speculative interventions (RLHF, interpretability, pause/slowdown governance), near-zero on present accountability work. The near_term_harms_reading file swaps the victim set to present-day harmed populations and relocates ε to unchecked deployment; the dual_priority_reading file dissolves the crowding-out structure by authorizing both portfolios. The upstream reading (this one) currently dominates funding and legitimacy and therefore shapes its siblings' resource environment. Relation structure: this reading and the near-term reading make exclusive definitional claims over the same predicate — no single framework can hold both definitions, which is the genuine rare forecloses case; the dual-priority reading is a portfolio claim this reading pressures (drains its resource base, forces its advocates onto x-risk terms) without making it unholdable, hence influences. Each family member links the others in its own affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
