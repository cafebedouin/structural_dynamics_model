% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Boundary on Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the legitimate-knowledge-boundary
 *   kernel: the experiential-pluralism rule that legitimate knowledge arises
 *   from lived experience and community validation, with methodological
 *   standards demoted to one tool among many. The arrangement the story is
 *   about is that boundary itself — who gains standing as a knower, whose
 *   validation counts, and what claims may guide action without
 *   methodological sign-off. It grew from the 1970s health-social-movements
 *   onward (women's health, AIDS activism, environmental justice,
 *   service-user movements) into institutionalized participatory machinery:
 *   community review panels, mandated patient involvement in research,
 *   co-produced policy evidence. The claim/metric gap is deliberate and
 *   independent: the arrangement is CLAIMED as tangled_rope because it
 *   possesses both a genuine coordination function (admitting situated
 *   knowledge that credentialed review systematically misses; distributing
 *   trust-allocation so no single institution monopolizes legitimacy) and
 *   real asymmetric extraction channels (error-cost externalities pushed onto
 *   reliance populations, suppression of intra-community dissent, opportunist
 *   harvesting of credibility by low-barrier entrants). Per the
 *   epsilon-invariance principle, the sibling readings are separate
 *   constraints in separate files: the credentialed_expertise_reading authors
 *   epsilon for the credential-gatekeeping arrangement, the
 *   hybrid_coproduction_reading for the integration arrangement, and this
 *   file authors epsilon only for the experience-primacy arrangement as this
 *   reading assesses it. The three files are linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - community_validation_facilitators: Agenda-setting beneficiary ([organized]/[identity_locked]) — administers validation criteria and runs the participatory machinery; careers and grant lines fused with the arrangement's continuation
 *   - lived_experience_knowers: Primary beneficiary ([organized]/[constrained]) — testimony gains formal standing; standing evaporates outside the community processes
 *   - marginalized_community_members: Dual-positioned beneficiary/payer ([powerless]/[trapped]) — first-time agenda access, but carries community-consensus errors and majority override of minority experience
 *   - misinformation_entrepreneurs: Opportunist beneficiary ([moderate]/[mobile]) — monetizes attention and product sales wherever validation barriers are lowest; exits any community that closes to them
 *   - credentialed_experts: Payer with arbitrage ([institutional]/[arbitrage]) — certification no longer confers standing by itself; can rebrand expertise as context-specific or join participatory processes
 *   - intra_community_dissenters: Trapped payer ([powerless]/[identity_locked]) — divergent experience reads as betrayal; exit costs belonging and identity
 *   - high_stakes_reliance_populations: Diffuse-error payer ([powerless]/[trapped]) — must act on whatever account carries local legitimacy; absorbs failures of community-validated claims
 *   - research_funding_bodies: Analytical observer ([institutional]/[analytical]) — conditions grant streams on demonstrated community involvement and evaluates which validation regime produces usable results
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.48).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.38).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Boundary on Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, 'a036f104-a404-4edb-abff-2bfc830110c8').
narrative_ontology:cs_kernel_codification('a036f104-a404-4edb-abff-2bfc830110c8', distributed).
narrative_ontology:cs_authority_grounding('a036f104-a404-4edb-abff-2bfc830110c8', practice).
narrative_ontology:cs_interpretation_layer_present('a036f104-a404-4edb-abff-2bfc830110c8').
narrative_ontology:cs_reading_relation('a036f104-a404-4edb-abff-2bfc830110c8', legitimate_knowledge_boundary__credentialed_expertise_reading, forecloses).
narrative_ontology:cs_reading_relation('a036f104-a404-4edb-abff-2bfc830110c8', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('a036f104-a404-4edb-abff-2bfc830110c8', foundational, lived_experience_epistemic_sufficiency).
narrative_ontology:cs_axiom_status(lived_experience_epistemic_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a036f104-a404-4edb-abff-2bfc830110c8', lived_experience_epistemic_sufficiency, deontological).
narrative_ontology:cs_axiom('a036f104-a404-4edb-abff-2bfc830110c8', foundational, community_validation_confers_legitimacy).
narrative_ontology:cs_axiom_status(community_validation_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a036f104-a404-4edb-abff-2bfc830110c8', community_validation_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('a036f104-a404-4edb-abff-2bfc830110c8', secondary, methodological_standards_are_one_tool_among_many).
narrative_ontology:cs_axiom_status(methodological_standards_are_one_tool_among_many, holdable).
narrative_ontology:cs_axiom_grounding('a036f104-a404-4edb-abff-2bfc830110c8', methodological_standards_are_one_tool_among_many, instrumental).
narrative_ontology:cs_reference_frame('a036f104-a404-4edb-abff-2bfc830110c8', situated_knowledge_primacy_norm).
narrative_ontology:cs_drift_state('a036f104-a404-4edb-abff-2bfc830110c8', contemporary_participatory_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a036f104-a404-4edb-abff-2bfc830110c8', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_knowers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_community_members).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validation_facilitators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, misinformation_entrepreneurs).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, high_stakes_reliance_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_community_members).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_injustice_critique).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_theory).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the participatory machinery: convene community review panels, train lived-experience contributors, set the criteria by which a claim counts as community-validated, and decide which experiential reports reach guideline committees, funders, and commissioners. Salaries and grant lines now depend on keeping these processes running. Leaving the field means abandoning careers built entirely inside participatory research networks and the professional identity that came with them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_validation_facilitators, agenda_setter,
    organized, biographical, identity_locked, regional).

% Contribute testimony drawn from their own lives — illness trajectories, housing conditions, service encounters — and see it cited in guidelines, funding calls, and legal submissions in ways that were previously dismissed as anecdote. Their standing exists only inside the community-validation processes; stepping outside them returns their accounts to anecdotal status, so participation is continuous rather than optional.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_knowers, beneficiary,
    organized, biographical, constrained, regional).

% Gain first-time access to defining the problems that research and policy address — environmental exposure, policing, benefits administration — through accounts their communities validate collectively. When the community consensus settles on a wrong account, they carry the consequences alongside everyone else, and experiences that diverge from the majority's can be overridden inside the very processes that admitted them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_community_members, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_community_members, payer).

% Operate where validation barriers are lowest: package compelling experiential narratives, build audiences around them, and monetize the attention and product sales that borrow the authority of community endorsement. They invest nothing in the communities whose credibility they borrow and move freely to new audiences whenever a community or platform closes to them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, misinformation_entrepreneurs, beneficiary,
    moderate, immediate, mobile, global).

% Hold advanced training and publish under methodological review; under this boundary their certification no longer by itself confers standing, and they must re-enter legitimacy through participation processes or reframe their expertise as context-specific. Decades of specialized training lose part of their scarcity value, and claims that would not survive methodological scrutiny circulate with community endorsement attached. Their arbitrage path — converting credentials into contextual-expertise currency and joining co-production formats — softens but does not eliminate the loss.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    institutional, generational, arbitrage, global).

% Members whose own experience contradicts the community's settled account — the patient who recovered on the discontinued treatment, the resident whose street saw different harms than the campaign describes. Speaking up costs them belonging: the community reads their divergence as betrayal or inauthenticity, allies distance themselves, and leaving would surrender the relationships and shared identity that anchor their lives, so most fall silent and some stop trusting their own perceptions.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Patients choosing treatments, tenants facing eviction, claimants navigating benefits systems — people who must act on whatever account of their situation carries local legitimacy. They cannot opt out of the epistemic environment their care and cases run on, they had no seat in producing the community-validated claims that guide those decisions, and they absorb the full consequences when those claims fail.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, high_stakes_reliance_populations, payer,
    powerless, biographical, trapped, national).

% Public and philanthropic funders watching the contest between validation regimes from outside it: they condition grant streams on demonstrated community involvement, commission evaluations comparing participatory and conventional methods, and shift resources toward whichever boundary produces usable results. They neither run community validation nor bear its error costs directly, which gives them the closest thing to an analytical seat in the dispute.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, research_funding_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a validation route for situated knowledge that credentialed review systematically misses — patient-reported outcome signals, community ecological observation, service-user insight into how interventions actually land — and distributes trust-allocation across communities so that no single institution monopolizes the determination of what counts as known.
% TRANSFER_FUNCTION: Moves epistemic authority and agenda-setting power from credentialed institutions toward communities and experienced knowers; moves research attention and funding toward community-defined problems; and correspondingly moves decision-risk onto whoever must act on community-validated claims — reliance populations, clinicians, officials — without their consent.
% ABSENT_VOICES: Intra-community dissenters are present in the communities but absent from the conversation: consensus rules recode their divergent experience as inauthenticity rather than testimony. Methodological minorities inside adopting institutions (biostatisticians, trialists who want experiential claims triangulated before deployment) are heard politely and overruled. Future people who will inherit today's validated errors have no seat anywhere. None of these seats drives classification — they mark where unanimity about the boundary's fairness is an artifact of who was in the room.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, participatory medicine panels, community-based research programs, patient-involvement mandates, and citizen-science legitimating frames would lose their warrant; thousands of currently admissible contributions would revert to anecdote; funders would reroute involvement-conditioned grant streams; and credentialed gatekeeping would reassert monopoly over what counts as evidence. Careers, institutions, and entire evidence pipelines are arranged around the boundary's continuation.
% FOUNDING_PROBLEM: Credentialed expertise had systematically dismissed situated knowledge: patients were told their symptoms were not real, communities' environmental observations were ruled inadmissible, service users were treated as objects of study rather than sources of insight — and the gatekeeping that produced these exclusions also produced blind spots and institutional capture.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of medicine and STS scholars (with no stake in the participatory apparatus) have documented the excluded-situated-knowledge record, including how lay AIDS activism's treatment-literacy forced trial-design changes mainstream researchers later adopted; clinical-methods researchers independent of advocacy organizations have published on how patient-reported outcomes measurably improved measurement validity; and philosophers of science who reject the pluralist remedy nonetheless concede in print that the exclusion was real. The founding problem's liveness is attested by continuing exclusion findings in funding-body equity reviews — sources outside the beneficiary set.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) rather than low because the boundary's costs are real but mostly externalized: error costs land on reliance populations who did not participate in producing the claims, dissenters pay in voice and belonging, and credentialed experts pay in devalued training — while the largest gains (standing, agenda access) are widely distributed rather than concentrated. Suppression (0.38) is authored as a raw structural property, unscaled by power or scope: the boundary suppresses through consensus rules, facilitator gatekeeping, and community sanction rather than state machinery, and it leaves the credentialed alternative fully live (accessibility_collapse 0.35 — alternatives persist, which is why resistance is substantial at 0.60: professional bodies, journal editors, and methodologists actively contest the boundary). Theater (0.36) reflects the growing share of participation that is tokenistic — panels convened for compliance rather than power transfer. The measurement series run on ONE shared grid (1970/1985/1995/2005/2015/2025) with every tracked metric authored at every point; the trajectories are monotonic, not cyclical — institutionalization ratchets enforcement capacity upward (suppression_requirement rising from 0.15 to 0.38 as participation became a funding condition), accumulates extraction (epsilon 0.22 to 0.48 as the opportunist economy and error stock grew), and substitutes proxy goals (theater 0.12 to 0.36 as involvement became a box to tick). Coalition note: the three payer seats are individually powerless, but reliance populations and dissenters lack even latent coalition infrastructure — their harms are private, dispersed, and identity-costly to voice — whereas experts retain organized counter-pressure, which is why resistance registers despite diffuse payer weakness. Identity-lock dynamics bind two seats from opposite directions: facilitators through professional/institutional identity fusion (the participatory mission has become who they are; breaking the frame would strand careers), dissenters through relational identity (community belonging constitutes the self; exit is unthinkable even under sustained dismissal).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different arrangements from identical structural facts. From the facilitator and knower seats, the boundary is a long-overdue correction: it admits testimony that saved lives (lay epidemiology reshaping trial design) and checks institutional capture. From the credentialed-expert seat, the same boundary dissolves legitimately earned authority and forces re-entry through processes it does not control — though its arbitrage exit (reframing as context-specific expertise) damps its effective burden relative to trapped payers. From the reliance-population seat, the boundary is a risk transfer it never consented to: decisions about its body and benefits now rest on validation it cannot audit. From the dissenter seat, the boundary is a new orthodoxy with the old exclusion's mechanics. The engine computes these per-seat divergences from the declared power, exit, and directional data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: lived_experience_knowers and marginalized_community_members sit near the subsidized end (the boundary confers standing and agenda access on them), community_validation_facilitators slightly above them (they collect roles and funding but also bear the labor of maintaining the machinery), and misinformation_entrepreneurs nearest zero (pure opportunistic subsidy, mobile exit, no maintenance burden). Victim declarations map to high-directionality seats: intra_community_dissenters and high_stakes_reliance_populations sit near the full-target end (trapped or identity-locked, bearing voice-loss and error costs with no offsetting gain), while credentialed_experts sit mid-high — they pay real costs in devalued credentials and unreviewed rivals' claims, but their institutional power and arbitrage exit (converting credentials into contextual-expertise currency) damp effective extraction below what trapped payers experience. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already separate the seats correctly, and the coarse power-atom keying of overrides would misapply a single correction across the three distinct powerless seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credentialed gatekeeping systematically dismissing situated knowledge and producing blind spots and capture — is live, so this is not a mandatrophy case: the arrangement has not outlived its function. The classification discipline matters here in both directions. Reading the boundary as pure coordination (rope) would erase the measurable extraction channels: error-cost externalities, dissent suppression, and opportunist credibility-harvesting are not coordination overhead but asymmetric burdens with identifiable bearers. Reading it as pure extraction (snare) would erase the coordination core: no seat captures the arrangement's yields systematically, gains are diffuse, and the inclusion function demonstrably produces knowledge credentialed review misses. The tangled_rope claim holds both truths in one structure. The forward risk is drift, not decay: if facilitator professionalization and compliance-driven participation continue their trajectory, theater keeps rising and the arrangement's margins go piton-shaped (performative panels maintained because defunding them costs more than tolerating them) even while its core function persists — the theater_ratio series is the early-warning indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the legitimate_knowledge_boundary kernel (instantiating experiential_pluralism_reading); what would the sibling readings change structurally if adopted in place of this one?',
    'Compile and classify the sibling reading stories separately (legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading) and diff their beneficiary/victim sets, epsilon values, and enforcement profiles against this file.',
    'The credentialed_expertise_reading concentrates validation in expert institutions, raising suppression and shifting beneficiaries toward professional bodies; the hybrid_coproduction_reading raises coordination overhead while damping both error-risk and exclusion. Adoption of either sibling moves this boundary''s classification along different axes; the disagreement between readings is located in whether methodological credentialing is NECESSARY for legitimacy (credentialed reading) versus merely one tool among many (this reading) versus jointly required with experiential validity (hybrid reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Reading-indexed identity: sibling readings instantiate different constraints from the same kernel, with different epsilon and different victim sets.').

omega_variable(
    error_cost_allocation_ambiguity,
    'Are the error costs borne by populations relying on community-validated claims an intrinsic price of distributed validation, or an artifact of weak implementation (missing triangulation, missing escalation paths to methodological review)?',
    'Compare error rates and harm distributions across participatory programs that embed structured triangulation practices against those that rely on raw community endorsement.',
    'If the costs are an implementation artifact, effective extraction is lower than measured and the arrangement trends toward pure coordination; if intrinsic to weighting experiential claims equally or higher than methodological ones, the payer-side asymmetry is structural and permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_cost_allocation_ambiguity, empirical, 'Whether distributed validation''s error costs are intrinsic or implementational.').

omega_variable(
    community_capture_dynamics,
    'Does community validation concentrate into new gatekeeping elites — professionalized facilitators, vocal majorities, platform curators — reproducing the exclusion this boundary was built to remove?',
    'Longitudinal study of who sets validation criteria inside mature participatory programs, and turnover analysis of whose testimony actually changes funded agendas.',
    'If capture is real, the agenda-setter seat drifts toward extractor position and the arrangement hardens at that seat; if validation criteria remain genuinely rotating and porous, the coordination function dominates and extraction stays secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_capture_dynamics, empirical, 'Whether distributed validation regenerates concentrated gatekeeping.').

omega_variable(
    dissent_suppression_mechanism,
    'Is the silencing of intra-community dissenters primarily structural (consensus rules, facilitator gatekeeping, social sanction) or internalized (members pre-filtering their own divergent experience as inauthentic)?',
    'Post-exit testimony studies: track whether former members recover willingness to voice divergent experience after leaving the community, or carry the self-censorship with them.',
    'If internalized, the effective suppression experienced by dissenters exceeds the structural measure — the constraint travels with the agent after exit — and remedies aimed at procedural rules alone will underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, empirical, 'Structural versus internalized conformity pressure inside validating communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkbp_epr_tr_t1970, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(lkbp_epr_tr_t1970, observed).
narrative_ontology:measurement(lkbp_epr_tr_t1985, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement_basis(lkbp_epr_tr_t1985, observed).
narrative_ontology:measurement(lkbp_epr_tr_t1995, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1995, 0.21).
narrative_ontology:measurement_basis(lkbp_epr_tr_t1995, observed).
narrative_ontology:measurement(lkbp_epr_tr_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement_basis(lkbp_epr_tr_t2005, observed).
narrative_ontology:measurement(lkbp_epr_tr_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement_basis(lkbp_epr_tr_t2015, observed).
narrative_ontology:measurement(lkbp_epr_tr_t2025, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2025, 0.36).
narrative_ontology:measurement_basis(lkbp_epr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(lkbp_epr_be_t1970, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement_basis(lkbp_epr_be_t1970, observed).
narrative_ontology:measurement(lkbp_epr_be_t1985, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement_basis(lkbp_epr_be_t1985, observed).
narrative_ontology:measurement(lkbp_epr_be_t1995, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement_basis(lkbp_epr_be_t1995, observed).
narrative_ontology:measurement(lkbp_epr_be_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement_basis(lkbp_epr_be_t2005, observed).
narrative_ontology:measurement(lkbp_epr_be_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement_basis(lkbp_epr_be_t2015, observed).
narrative_ontology:measurement(lkbp_epr_be_t2025, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(lkbp_epr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(lkbp_epr_su_t1970, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement_basis(lkbp_epr_su_t1970, observed).
narrative_ontology:measurement(lkbp_epr_su_t1985, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement_basis(lkbp_epr_su_t1985, observed).
narrative_ontology:measurement(lkbp_epr_su_t1995, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1995, 0.26).
narrative_ontology:measurement_basis(lkbp_epr_su_t1995, observed).
narrative_ontology:measurement(lkbp_epr_su_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement_basis(lkbp_epr_su_t2005, observed).
narrative_ontology:measurement(lkbp_epr_su_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement_basis(lkbp_epr_su_t2015, observed).
narrative_ontology:measurement(lkbp_epr_su_t2025, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(lkbp_epr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the legitimate knowledge boundary' into three structurally distinct readings, per the epsilon-invariance principle. The label conflates three rival boundary rules with different epsilon values, different beneficiary/victim sets, and different failure modes: credential-gatekeeping (concentrated validation, institutional beneficiaries, excluded-experience victims), experience-primacy (this file: distributed validation, diffuse beneficiary gains, error-externality and dissent victims), and enforced integration (highest coordination overhead, damped extremes on both sides). Genealogically, the credentialed arrangement is upstream — its exclusions supplied the founding problem this reading answers — and this reading is upstream of the hybrid reading, whose co-production machinery presupposes the participatory infrastructure the pluralist movement built. Each file links its siblings through affects_constraints; classification of the kernel-level dispute requires reading all three together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
